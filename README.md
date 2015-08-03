Repository for ESMF and NUOPC related training materials.


SingleCompWithDriver
====================
An example ESMF application with a single ESMF Component
and simple AppDriver.


SingleCompPartialDriver
=======================
An example ESMF application with a single ESMF Component
and only a partically complete driver.  Completing the
driver is left an exercise.


SingleCompFieldHalo
===================
A simple 2D heat diffusion ESMF Component that shows how
to perform halo communication on a Field.


SingleCompFieldHaloMissing
==========================
A simple 2D heat diffusion ESMF Component that is missing
calls to perform halo communication, resulting in
an incorrect computation.  Adding code to set up the halo
operation using ESMF is left as an exercise.
