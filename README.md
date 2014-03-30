# inkwell-playground

A sample project for playing with Inkwell.

## Usage

Once you've started a REPL for the project, you can start the sketch with
`user/start!`. Modify and re-evaluate `inkwell-playground.core/handle-event`
and `inkwell-playground.core/draw` and you'll see your changes immediately.

If you make a mistake and get an exception, Inkwell pauses the sketch. To
unpause it, call `user/resume!`. To completely reload all namespaces and
restart the sketch, call `user/restart!`.

## License

Copyright © 2014 [Solita](http://www.solita.fi)

Distributed under the Eclipse Public License either version 1.0 or (at your
option) any later version.
