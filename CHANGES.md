## unreleased

## 0.3 - 2026-01-16

- ensure proper broadcasting to avoid deadlock
- use a `Mutex.t` instead of `Atomic.t`

## 0.2 - 2025-12-19

- simplify API
- reduce contention
- make sure `None` is return in all cases as soon as the synchronizer is closed

## 0.1 - 2025-10-03

- first release
