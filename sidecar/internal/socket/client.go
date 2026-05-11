package socket

import (
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
}

func New(path string) *Client {
	return &Client{path: path, lastSent: make(map[string]string)}
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
			return nil
		}
		if attempt >= 20 {
			return fmt.Errorf("connect %s: %w", c.path, err)
		}
		time.Sleep(500 * time.Millisecond)
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
