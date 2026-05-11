package control

import "sync"

// State holds per-resource control overrides (currently just display limits).
// Renderers read from it; the socket reader writes to it.
type State struct {
	mu      sync.RWMutex
	limits  map[string]int           // alias -> override limit (0 means no limit)
	wakes   map[string]chan struct{} // alias -> non-blocking wake channel
}

func New() *State {
	return &State{
		limits: make(map[string]int),
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

// SetLimit stores LIMIT for ALIAS and wakes the resource loop (if any) to re-render.
// A LIMIT of 0 means "no limit"; -1 (or absent) means "use the renderer's default".
func (s *State) SetLimit(alias string, limit int) {
	s.mu.Lock()
	s.limits[alias] = limit
	ch := s.wakes[alias]
	s.mu.Unlock()
	if ch != nil {
		select {
		case ch <- struct{}{}:
		default:
		}
	}
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
