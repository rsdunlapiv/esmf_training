!------------------------------------------------------------------------------
!  AppDriver.F90 - Main program for an ESMF Application with a single
!    Gridded Component.
!
!  Creates the top Gridded Component and calls the Initialize, Run,
!  and Finalize routines for it.
!

program AppDriver

  ! ESMF module, defines all ESMF data types and procedures
  use ESMF

  ! Gridded Component registration routines.
  use ATM, only : ATM_SetServices => SetServices

  implicit none

  !---------------------------------------------------------------------------
  !  Define local variables
  !---------------------------------------------------------------------------

  ! Components and States
  type(ESMF_GridComp) :: compGridded
  type(ESMF_State) :: importState
  type(ESMF_State) :: exportState

  ! A Grid object to be passed to the component
  type(ESMF_Grid) :: grid

  ! A Clock, a Calendar, and timesteps
  type(ESMF_Clock) :: clock
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Time) :: startTime
  type(ESMF_Time) :: stopTime
  type(ESMF_Time) :: currTime

  ! Variables related to the Grid
  integer :: i_max, j_max

  ! Return codes for error checks
  integer :: localrc, userrc

  !---------------------------------------------------------------------------
  !  Initialize ESMF.  Note that an output Log is created by default.
  !---------------------------------------------------------------------------

  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("ESMF AppDriver start", ESMF_LOGMSG_INFO)

  !---------------------------------------------------------------------------
  !  Create the top Gridded Component.
  !---------------------------------------------------------------------------

  compGridded = ESMF_GridCompCreate(name="ESMF Gridded Component", &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("Component Create finished", ESMF_LOGMSG_INFO)

  !----------------------------------------------------------------------------
  !  Register the set services method for the top Gridded Component.
  !----------------------------------------------------------------------------

  call ESMF_GridCompSetServices(compGridded, userRoutine=ATM_SetServices, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg="Registration failed")) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !  Create and initialize a Clock.
  !----------------------------------------------------------------------------

  call ESMF_TimeIntervalSet(timeStep, s=3600, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeSet(startTime, yy=2004, mm=9, dd=25, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_TimeSet(stopTime, yy=2004, mm=9, dd=26, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  clock = ESMF_ClockCreate(timeStep, startTime, stopTime=stopTime, &
    name="Application Clock", rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !  Create and initialize a Grid.
  !
  !  The default lower indices for the Grid are (/1,1/).
  !  The upper indices for the Grid are set to (/10,40/).  This means a Grid
  !  will be created with 10 grid cells in the x direction and 40 grid cells
  !  in the y direction.  No coordinates are set.
  !----------------------------------------------------------------------------

  i_max = 10
  j_max = 40

  grid = ESMF_GridCreateNoPeriDim(maxIndex=(/i_max, j_max/), &
    name="source grid", rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  ! Attach the grid to the Component
  call ESMF_GridCompSet(compGridded, grid=grid, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !  Create and initialize States to use for import and export from/to
  !  the child gridded component.
  !----------------------------------------------------------------------------

  importState = ESMF_StateCreate(name="atmImport", rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  exportState = ESMF_StateCreate(name="atmExport", rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !  Call the initialize, run, and finalize methods of the top component.
  !  When the initialize method of the top component is called, it will in
  !  turn call the initialize methods of all its child components, they
  !  will initialize their children, and so on.  The same is true of the
  !  run and finalize methods.
  !----------------------------------------------------------------------------

  call ESMF_GridCompInitialize(compGridded, importState=importState, &
    exportState=exportState, clock=clock, userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(userrc, msg="Initialize failed in component code")) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(localrc, msg="Initialize failed")) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  print *, "START RUN LOOP"
  !call ESMF_ClockGet(clock, currTime=currTime)
  call ESMF_ClockPrint(clock, options="currTime string")

  do while (.not. ESMF_ClockIsStopTime(clock))
    call ESMF_GridCompRun(compGridded, importState=importState, &
      exportState=exportState, clock=clock, userRc=userrc, rc=localrc)
    if (ESMF_LogFoundError(userrc, msg="Run failed in component code")) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(localrc, msg="Run failed")) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

    call ESMF_ClockAdvance(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, msg="Run failed")) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  end do

  print *, "END RUN LOOP"
  call ESMF_ClockPrint(clock, options="currTime string")


  call ESMF_GridCompFinalize(compGridded, importState=importState, &
    exportState=exportState, clock=clock, userRc=userrc, rc=localrc)
  if (ESMF_LogFoundError(userrc, msg="Finalize failed in component code")) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(localrc, msg="Finalize failed")) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !  Destroy objects.
  !----------------------------------------------------------------------------

  call ESMF_ClockDestroy(clock, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(importState, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_StateDestroy(exportState, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  call ESMF_GridCompDestroy(compGridded, rc=localrc)
  if (ESMF_LogFoundError(localrc, msg=ESMF_LOGERR_PASSTHRU, &
    file=__FILE__, line=__LINE__)) &
    call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)

  !----------------------------------------------------------------------------
  !  Finalize and clean up.
  !----------------------------------------------------------------------------

  call ESMF_Finalize()

end program AppDriver
