program ESMF_MeshTest

  use ESMF

  implicit none

  type(ESMF_Mesh) :: mesh
  integer :: rc
  integer :: numNodes
  integer, pointer :: nodeIds(:)
  integer, pointer :: nodeOwners(:)
  integer, pointer :: elemConn(:), elemIds(:), elemTypes(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)

  integer :: numQuadElems, numTriElems, numTotElems

  integer :: localPET
  integer :: elb(1), eub(1)

  type(ESMF_VM) :: vm
  type(ESMF_Field) :: field
  type(ESMF_DistGrid) :: distgrid
  integer, pointer :: indexList(:)
  integer :: numOwnedNodes, numOwnedElements
  real(ESMF_KIND_R8), pointer :: ownedNodeCoords(:), ownedElemCoords(:)


  call ESMF_Initialize(rc=rc, vm=vm)

  call ESMF_VMGet(vm, localPet=localPet)

  ! Break up what's being set by PET
  if (localPET .eq. 0) then !!! This part only for PET 0
    ! Set number of nodes
    numNodes=4

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/1,2,4,5/)

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/0.0,0.0, & ! node id 1
      1.0,0.0, & ! node id 2
      0.0,1.0, & ! node id 4
      1.0,1.0 /) ! node id 5

    ! Allocate and fill the node owner array.
    allocate(nodeOwners(numNodes))
    nodeOwners=(/0, & ! node id 1
      0, & ! node id 2
      0, & ! node id 4
      0/)  ! node id 5

    ! Set the number of each type of element, plus the total number.
    numQuadElems=1
    numTriElems=0
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/1/)

    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

    ! Allocate and fill the element connection type array.
    ! Note that entry are local indices
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,4,3/) ! elem id 1

  else if (localPET .eq. 1) then !!! This part only for PET 1
    ! Set number of nodes
    numNodes=4

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/2,3,5,6/)

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/1.0,0.0, & ! node id 2
      2.0,0.0, & ! node id 3
      1.0,1.0, & ! node id 5
      2.0,1.0 /) ! node id 6

    ! Allocate and fill the node owner array.
    allocate(nodeOwners(numNodes))
    nodeOwners=(/0, & ! node id 2
      1, & ! node id 3
      0, & ! node id 5
      1/)  ! node id 6

    ! Set the number of each type of element, plus the total number.
    numQuadElems=0
    numTriElems=2
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/2,3/)

    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
      ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

    ! Allocate and fill the element connection type array.
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,3, & ! elem id 2
      2,4,3/)  ! elem id 3

  else if (localPET .eq. 2) then !!! This part only for PET 2
    ! Set number of nodes
    numNodes=4

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/4,5,7,8/)

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/0.0,1.0, & ! node id 4
      1.0,1.0, & ! node id 5
      0.0,2.0, & ! node id 7
      1.0,2.0 /) ! node id 8

    ! Allocate and fill the node owner array.
    ! Since this Mesh is all on PET 0, it's just set to all 0.
    allocate(nodeOwners(numNodes))
    nodeOwners=(/0, & ! node id 4
      0, & ! node id 5
      2, & ! node id 7
      2/)  ! node id 8

    ! Set the number of each type of element, plus the total number.
    numQuadElems=1
    numTriElems=0
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/4/)

    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

    ! Allocate and fill the element connection type array.
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,4,3/) ! elem id 4

  else if (localPET .eq. 3) then !!! This part only for PET 3
    ! Set number of nodes
    numNodes=4

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/5,6,8,9/)

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/1.0,1.0, &  ! node id 5
      2.0,1.0, &  ! node id 6
      1.0,2.0, &  ! node id 8
      2.0,2.0 /)  ! node id 9

    ! Allocate and fill the node owner array.
    allocate(nodeOwners(numNodes))
    nodeOwners=(/0, & ! node id 5
      1, & ! node id 6
      2, & ! node id 8
      3/)  ! node id 9

    ! Set the number of each type of element, plus the total number.
    numQuadElems=1
    numTriElems=0
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/5/)

    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

    ! Allocate and fill the element connection type array.
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,4,3/) ! elem id 5
  endif


  ! Create Mesh structure in 1 step
  mesh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, &
    nodeIds=nodeIds, nodeCoords=nodeCoords, &
    nodeOwners=nodeOwners, elementIds=elemIds,&
    elementTypes=elemTypes, elementConn=elemConn, &
    rc=rc)


  ! After the creation we are through with the arrays, so they may be
  ! deallocated.
  deallocate(nodeIds)
  deallocate(nodeCoords)
  deallocate(nodeOwners)
  deallocate(elemIds)
  deallocate(elemTypes)
  deallocate(elemConn)


  ! At this point the mesh is ready to use. For example, as is
  ! illustrated here, to have a field created on it. Note that
  ! the Field only contains data for nodes owned by the current PET.
  ! Please see Section "Create a Field from a Mesh" under Field
  ! for more information on creating a Field on a Mesh.
  field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, rc=rc)


  call ESMF_MeshGet(mesh, nodalDistgrid=distgrid, &
    numOwnedNodes=numOwnedNodes, &
!    numOwnedElements=numOwnedElements, &
    rc=rc)

  allocate(ownedNodeCoords(2*numOwnedNodes))
!  allocate(ownedElemCoords(2*numOwnedElements))

  call ESMF_MeshGet(mesh, nodalDistgrid=distgrid, &
    ownedNodeCoords=ownedNodeCoords, &
!    ownedElemCoords=ownedElemCoords, &
    rc=rc)

  print *, "localPet=", localPet, " first node coords = ", ownedNodeCoords(1), ownedNodeCoords(2)
  !print *, "localPet=", localPet, " first elem coords = ", ownedElemCoords(1), ownedElemCoords(2)

  !allocate(indexList(numOwnedNodes))

    ! call ESMF_DistGridGet(distgrid, localDE=0, &
    !   seqIndexList=indexList, rc=rc)

  !print *, "localPet=", localPet, " indexList = ", indexList(:)

  !deallocate(indexList)

  deallocate(ownedNodeCoords)
  !deallocate(ownedElemCoords)

  call ESMF_Finalize()


end program

