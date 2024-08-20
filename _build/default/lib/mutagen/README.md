# Mutagen

Mutagen is a simple mutex server for the [Nitrogen Web
Framework](http://nitrogenproject.com).  It was previously part of [Nitro
Cache](https://github.com/nitrogen/nitro_cache) but has been pulled out and
generalized in case a more generalized mutex system is needed for Erlang and/or
Nitrogen users.

## Add to your rebar.config

```erlang
{deps, [
	mutagen
]}.
```

## Make sure you start it

Either:

* Add it to the `applications` list in your `.app.src` file, or
* Start it manually with `application:ensure_all_started(mutagen).`

## Usage

Mutagen has a few very simple functions, once the server is running.

* `lock(Key) -> fail | success`:  Attempt to lock the mutex `Key`. If the mutex
  is successfully locked, the call will return `success`.  If the mutex is
  already locked, the mutex will return `fail`.

* `lock(Key, Timeout) -> fail | success`: Attempt the lock mutex `Key`,
  however, if the mutex is currently already locked, the call will stall and
  wait up to `Timeout` milliseconds before giving up and returning `fail`.

* `wait(Key) -> free`: Only return when the mutex identified by `Key` is free.
  This does not then lock the mutex, it merely returns when it's available.

* `wait(Key, Timeout) -> free | not_free`: Only returns `free` when the mutex
  identified by `Key` is free, or if the mutex is not free after `Timeout`
  milliseconds, return `not_free`.

* `status() -> StatusInfo`:  Returns a proplist of information related to the
  number of mutexes that are currently locked, the number of processes queued
  to lock an already locked mutex, and the number of processes waiting for the
  mutex to be free.

## About

Apache 2.0 License

Copyright 2023 Jesse Gumm
