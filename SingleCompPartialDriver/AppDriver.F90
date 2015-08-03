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

  ! A Clock, a Calendar, and timesteps
  type(ESMF_Clock) :: clock
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Time) :: startTime
  type(ESMF_Time) :: stopTime
  type(ESMF_Time) :: currTime

  ! Return codes for error checks
  integer :: localrc, userrc

  !---------------------------------------------------------------------------
  !  Initialize ESMF and set default calendar to ESMF_CALKIND_GREGORIAN.
  !---------------------------------------------------------------------------

  !! FIXME


  !---------------------------------------------------------------------------
  !  Create the top Gridded Component and assign to the variable compGridded.
  !---------------------------------------------------------------------------

  !! FIXME


  !----------------------------------------------------------------------------
  !  Register the set services method for the top Gridded Component.
  !----------------------------------------------------------------------------

  !! FIXME

  !----------------------------------------------------------------------------
  !  Set up startTime, stopTime, and timeStep.
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

  !----------------------------------------------------------------------------
  !  Create a clock using timeStep, startTime, and stopTime.
  !  Assign to local variable clock.
  !----------------------------------------------------------------------------

  !! FIXME

  !----------------------------------------------------------------------------
  !  Create and initialize States to use for import and export from/to
  !  the child gridded component.
  !  Assign created states to local variables importState and exportState.
  !----------------------------------------------------------------------------

  !! FIXME

  !----------------------------------------------------------------------------
  !  Call compGridded's initialize method, passing it the importState,
  !  exportState, and clock.
  !----------------------------------------------------------------------------

  !! FIXME

  print *, "START RUN LOOP"
  call ESMF_ClockPrint(clock, options="currTime string")

  do while (.not. ESMF_ClockIsStopTime(clock))

  !----------------------------------------------------------------------------
  !  Call compGridded's run method, passing it the importState,
  !  exportState, and clock.
  !----------------------------------------------------------------------------

    !! FIXME

    call ESMF_ClockAdvance(clock, rc=localrc)
    if (ESMF_LogFoundError(localrc, msg="Run failed")) &
      call ESMF_Finalize(rc=localrc, endflag=ESMF_END_ABORT)
  end do

  print *, "END RUN LOOP"
  call ESMF_ClockPrint(clock, options="currTime string")


  !----------------------------------------------------------------------------
  !  Call compGridded's finalize method, passing it the importState,
  !  exportState, and clock.
  !----------------------------------------------------------------------------

  !! FIXME



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
  !  Finalize the ESMF framework.
  !----------------------------------------------------------------------------

  !! FIXME

end program AppDriver
