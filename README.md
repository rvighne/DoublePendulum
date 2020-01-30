# DoublePendulum

A simulation of a chaotic double-pendulum system using Lagrangian mechanics.

We actually built this system and then inputted the real measurements of moment-of-inertia, center-of-mass, etc. into the code. The goal was to start both the real pendulum and this simulation at the same instant, and see for how long the two evolved consistently. Energy loss due to friction was not implemented, leading to some unnatural-looking results!

All the code is in [`src/Lib.hs`](src/Lib.hs). To build and run, use [Stack](https://www.haskellstack.org/); for example, `stack build` followed by `stack exec DoublePendulum-exe`. GLUT libraries need to be available; usually no problem on Linux, but for for Windows, [`freeglut.dll` from here](https://www.transmissionzero.co.uk/software/freeglut-devel/) works OK.

*A class project for Ryan Cardenas's AP Physics C at San Dieguito Academy (Spring 2018).*
