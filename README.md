Description
-----------

This is a game engine written in Common Lisp.

License
-------

MIT (see LICENSE.txt)

Installation from Github
------------------------

Games written with zr-game currently require SDL2 to run. You need to
have it installed on your machine before following the Lisp
installation instructions. You should be able to install it from your
package manager. You can also download it from:

  http://www.libsdl.org/download-2.0.php

The easiest way to handle all of the Common Lisp dependencies is to
have Quicklisp installed. To install it, you can follow the
instructions here:

  http://www.quicklisp.org/beta/

One way to have Quicklisp recognize zr-game is to do the following:

    cd ~/quicklisp/local-projects
    git clone https://github.com/zombie-raptor/zr-game.git

Once you place zr-game in the local-projects directory, you can load
it in your Lisp's REPL with:

    (ql:quickload 'zr-game)

If you want to run the example program to make sure that everything
works, you can launch it in the REPL with:

    (zr-game:example)
