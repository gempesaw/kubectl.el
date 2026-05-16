package control

import "sync"

// State holds per-resource control overrides set via socket messages from Emacs.
// Renderers read; the socket reader writes. All writes wake the resource loop
// (via a non-blocking channel) so the change shows up on the next render.
type State struct {
	mu     sync.RWMutex
	limits map[string]int           // alias -> override row limit (0 = no limit)
	sorts  map[string]string        // alias -> override sort column name
	wakes  map[string]chan struct{} // alias -> non-blocking wake channel
}

func New() *State {
	return &State{
		limits: make(map[string]int),
		sorts:  make(map[string]string),
		wakes:  make(map[string]chan struct{}),
	}
}

// Register reserves a wake channel for ALIAS. Returns the channel the resource loop
// should select on to know when its overrides have changed.
func (s *State) Register(alias string) <-chan struct{} {
	s.mu.Lock()
	defer s.mu.Unlock()
	ch, ok := s.wakes[alias]
	if !ok {
		ch = make(chan struct{}, 1)
		s.wakes[alias] = ch
	}
	return ch
}

// wakeAndUnlock is a tail-call helper: snapshots the wake channel while still under
// the write lock, releases the lock, then nudges the channel non-blockingly.
func (s *State) wakeAndUnlock(alias string) {
	ch := s.wakes[alias]
	s.mu.Unlock()
	if ch != nil {
		select {
		case ch <- struct{}{}:
		default:
		}
	}
}

// SetLimit stores LIMIT for ALIAS and wakes the resource loop.
// A LIMIT of 0 means "no limit".
func (s *State) SetLimit(alias string, limit int) {
	s.mu.Lock()
	s.limits[alias] = limit
	s.wakeAndUnlock(alias)
}

// LimitOrDefault returns the override limit for ALIAS if set, otherwise FALLBACK.
func (s *State) LimitOrDefault(alias string, fallback int) int {
	s.mu.RLock()
	defer s.mu.RUnlock()
	if v, ok := s.limits[alias]; ok {
		return v
	}
	return fallback
}

// SetSort stores COLUMN as the sort column for ALIAS and wakes the resource loop.
func (s *State) SetSort(alias, column string) {
	s.mu.Lock()
	s.sorts[alias] = column
	s.wakeAndUnlock(alias)
}

// SortOrDefault returns the override sort column for ALIAS if set, otherwise FALLBACK.
func (s *State) SortOrDefault(alias, fallback string) string {
	s.mu.RLock()
	defer s.mu.RUnlock()
	if v, ok := s.sorts[alias]; ok && v != "" {
		return v
	}
	return fallback
}
