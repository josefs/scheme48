# Some thoughts on how to make a meta tail call

* How does one reify a port and turn it into syntax? Do we have to
  close all files and sockets when restarting the program? That would
  be unfortunate.

* There's a tight coupling between parsing and evaluation, which I
  perhaps should have forseen but didn't. It's worth keeping in mind.

