package socket

import (
	"bufio"
	"encoding/json"
	"fmt"
	"net"
	"sync"
	"time"
)

type Client struct {
	path     string
	mu       sync.Mutex
	conn     net.Conn
	lastSent map[string]string // buffer -> last contents we shipped, for skip-if-unchanged
	inbox    chan IncomingMessage
}

// IncomingMessage is the shape of control messages sent by Emacs back to the sidecar.
// "type" determines which fields are populated:
//   set_limit: alias, limit
//   set_sort:  alias, column
type IncomingMessage struct {
	Type   string `json:"type"`
	Alias  string `json:"alias"`
	Limit  int    `json:"limit"`
	Column string `json:"column"`
}

func New(path string) *Client {
	return &Client{
		path:     path,
		lastSent: make(map[string]string),
		inbox:    make(chan IncomingMessage, 16),
	}
}

// Inbox returns the channel of inbound control messages. The reader goroutine
// is started on first connect; messages arrive here.
func (c *Client) Inbox() <-chan IncomingMessage {
	return c.inbox
}

type message struct {
	Buffer   string `json:"buffer"`
	Contents string `json:"contents"`
}

func (c *Client) Send(buffer, contents string) error {
	c.mu.Lock()
	defer c.mu.Unlock()

	// Skip if the rendered content hasn't changed since last send. Cuts noise from
	// MODIFIED events that don't actually affect what gets displayed.
	if prev, ok := c.lastSent[buffer]; ok && prev == contents {
		return nil
	}

	payload, err := json.Marshal(message{Buffer: buffer, Contents: contents})
	if err != nil {
		return err
	}
	payload = append(payload, '\n')

	if c.conn == nil {
		if err := c.connectLocked(); err != nil {
			return err
		}
	}

	if _, err := c.conn.Write(payload); err != nil {
		c.conn.Close()
		c.conn = nil
		return fmt.Errorf("socket write: %w", err)
	}
	c.lastSent[buffer] = contents
	return nil
}

func (c *Client) connectLocked() error {
	for attempt := 0; ; attempt++ {
		conn, err := net.Dial("unix", c.path)
		if err == nil {
			c.conn = conn
			// Start a reader goroutine bound to this connection. Closes when conn closes.
			go c.readLoop(conn)
			return nil
		}
		if attempt >= 20 {
			return fmt.Errorf("connect %s: %w", c.path, err)
		}
		time.Sleep(500 * time.Millisecond)
	}
}

// readLoop reads newline-delimited JSON messages from the connection and forwards
// them to the inbox channel. Exits when the connection is closed.
func (c *Client) readLoop(conn net.Conn) {
	scanner := bufio.NewScanner(conn)
	scanner.Buffer(make([]byte, 64*1024), 1024*1024)
	for scanner.Scan() {
		line := scanner.Bytes()
		if len(line) == 0 {
			continue
		}
		var msg IncomingMessage
		if err := json.Unmarshal(line, &msg); err != nil {
			continue
		}
		select {
		case c.inbox <- msg:
		default:
			// inbox is full; drop the message rather than block the reader.
		}
	}
}

func (c *Client) Close() {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.conn != nil {
		c.conn.Close()
		c.conn = nil
	}
}
