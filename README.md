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


NUOPC_SingleModelProto
======================
A simple NUOPC prototype application with a single driver
and single model component.


NUOPC_AtmOcnProto
=================
A NUOPC prototype application with a Driver, two Models,
and two Connectors.


NUOPC_AtmOcnLndProto
====================
A NUOPC prototype application with a Driver, three Models,
and multiple Connectors to transfer data between Models.


NUOPC_AtmOcnLndProtoPartial
===========================
Same as NUOPC_AtmOcnLndProto, but only partially complete.
This is intended to be used as an exercise.  See the README
in that directory for more info.


SimpleMesh
==========
An ESMF application showing how to create a Mesh on 4 PETs.


