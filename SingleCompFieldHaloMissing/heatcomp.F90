!
! This module is a basic ESMF Component that models heat diffusion in 2D.
!
! The domain is a logically rectangular 2D grid of 100x100 cells.  The
! domain is divided into 8 decomposition elements, but the parallel
! computation is incorrect because halo operations have been left out.
!
! Look for "FIXME" in this file to find places that need to be changed
! to introduce the halo operation.
!

module HEAT

  use ESMF
  implicit none

  private
  type(ESMF_RouteHandle) :: rh

  public SetServices

contains

  subroutine SetServices(comp, rc)
    type(ESMF_GridComp)   :: comp   ! must not be optional
    integer, intent(out)  :: rc     ! must not be optional

    ! Set the entry points for standard ESMF Component methods
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
      userRoutine=Init, rc=rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
      userRoutine=Run, rc=rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)

    rc = ESMF_SUCCESS

  end subroutine

  subroutine Init(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp                   ! must not be optional
    type(ESMF_State)      :: importState            ! must not be optional
    type(ESMF_State)      :: exportState            ! must not be optional
    type(ESMF_Clock)      :: clock                  ! must not be optional
    integer, intent(out)  :: rc                     ! must not be optional

    type(ESMF_Grid) :: grid2d
    type(ESMF_Field) :: field
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    integer :: elb(2), eub(2)
    integer :: i, j

    print *, "Gridded Comp Init starting"

    ! Create a 2D logically rectangular grid
    grid2d = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/), &
      maxIndex=(/100,100/), regDecomp=(/4,2/), &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error creating grid", &
      file=__FILE__, line=__LINE__)) &
      return

    !---------------------------------------------------------------------------
    !  FIXME
    !
    !  Change the call below to add padding for a 1 cell width halo
    !  around the exclusive region in all directions.  Do this by
    !  setting the totalLWidth and totalUWidth parameters.
    !
    !  Ref manual page for ESMF_FieldCreate():
    !  http://www.earthsystemmodeling.org/esmf_releases/last_built/ESMF_refdoc/node5.html#SECTION05036500000000000000
    !---------------------------------------------------------------------------

    ! Create an empty field on the 2D grid
    field = ESMF_FieldCreate(grid2d, typekind=ESMF_TYPEKIND_R8, &
      name="temperature", &
      rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error creating field", &
      file=__FILE__, line=__LINE__)) &
      return

    ! Get data pointer and bounds of local exclusive region
    call ESMF_FieldGet(field, farrayPtr=dataPtr, &
      exclusiveLBound=elb, &
      exclusiveUBound=eub, &
      rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error retrieving data pointer", &
      file=__FILE__, line=__LINE__)) &
      return

    ! Initialize field
    do j=elb(2), eub(2)
      do i=elb(1), eub(1)
        if (i==1 .or. j==1 .or. i==100 .or. j==100) then
          dataPtr(i,j) = 50.0  ! boundary condition
        else
          dataPtr(i,j) = 10.0  ! field initially constant
        endif
      enddo
    enddo

    !---------------------------------------------------------------------------
    !  Store a halo operation for the temperature field.  An ESMF_RouteHandle
    !  has already been declared at the module level called "rh".
    !---------------------------------------------------------------------------

    !! FIXME


    ! Add to export state
    call ESMF_StateAdd(exportState, (/field/), rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error adding to exportState", &
      file=__FILE__, line=__LINE__)) &
      return

    print *, "Gridded Comp Init returning"

    rc = ESMF_SUCCESS

  end subroutine Init

  subroutine Run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    type(ESMF_Field) :: field
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    real(ESMF_KIND_R8), allocatable :: tmpPtr(:,:)
    integer :: elb(2), eub(2)
    integer :: i, j


    !print *, "Gridded Comp Run starting"

    !call ESMF_ClockPrint(clock, options="currTime string")

    ! retrieve field from exportState
    call ESMF_StateGet(exportState, "temperature", field, rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error retrieving field", &
      file=__FILE__, line=__LINE__)) &
      return

    !---------------------------------------------------------------------------
    !  Perform the pre-calculated halo operation using the ESMF_RouteHandle "rh".
    !---------------------------------------------------------------------------

    !! FIXME


    ! Get data pointer and bounds of local exclusive region
    call ESMF_FieldGet(field, farrayPtr=dataPtr, &
      exclusiveLBound=elb, &
      exclusiveUBound=eub, &
      rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error retrieving data pointer", &
      file=__FILE__, line=__LINE__)) &
      return

    ! allocate temp array
    allocate( tmpPtr(elb(1):eub(1), elb(2):eub(2)) )
    tmpPtr = dataPtr

    ! do computation
    do j=elb(2), eub(2)
      do i=elb(1), eub(1)
        if (i==1 .or. j==1 .or. i==100 .or. j==100) then
          cycle
        else
          tmpPtr(i,j) = 0.25*(dataPtr(i,j-1)+dataPtr(i,j+1)+ &
            dataPtr(i-1,j)+dataPtr(i+1,j))
        endif
      enddo
    enddo

    dataPtr = tmpPtr

    ! deallocate
    deallocate(tmpPtr)

    !print *, "Gridded Comp Run returning"

    rc = ESMF_SUCCESS

  end subroutine Run

  subroutine Finalize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    type(ESMF_Field) :: field
    type(ESMF_Grid) :: grid2d

    print *, "Gridded Comp Finalize starting"

    !---------------------------------------------------------------------------
    !  Release the ESMF_RouteHandle for the halo.
    !---------------------------------------------------------------------------

    !! FIXME



    ! retrieve temperature field
    call ESMF_StateGet(exportState, "temperature", field, rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error retrieving field", &
      file=__FILE__, line=__LINE__)) &
      return

    ! write temperature field
    call ESMF_FieldWrite(field, "temp.nc", rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error writing field", &
      file=__FILE__, line=__LINE__)) &
      return

    ! get grid from field
    call ESMF_FieldGet(field, grid=grid2d, rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error retrieving grid from field", &
      file=__FILE__, line=__LINE__)) &
      return

    ! destroy field
    call ESMF_FieldDestroy(field, rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error destroying field", &
      file=__FILE__, line=__LINE__)) &
      return

    ! destroy grid
    call ESMF_GridDestroy(grid2d, rc=rc)
    if (ESMF_LogFoundError(rc, msg="Error destroying grid", &
      file=__FILE__, line=__LINE__)) &
      return


    print *, "Gridded Comp Finalize returning"

    rc = ESMF_SUCCESS

  end subroutine Finalize



end module
