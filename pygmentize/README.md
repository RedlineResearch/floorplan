# Floorplan lexer for pygmentize

The Makefile here clones the [pygments repo](https://github.com/pygments/pygments)
and sets up the Floorplan lexer for use with the `pygmentize` command. To build
and pygmentize do the following:

```bash
$ cd pygmentize && make all
...
$ export PYTHONPATH=`pwd`/inst/lib/python$(python --version | cut -f2 -d' ' | cut -f1,2 -d'.')/site-packages
$ ./inst/bin/pygmentize -l floorplan ../examples/immix/src/heap/layout.flp
```

