!
! A basic ESMF gridded component
!

module ATM

  use ESMF
  implicit none
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

    print *, "Gridded Comp Init starting"

    ! This is where the model specific setup code goes.

    ! The import and export states can be filled here
    !call ESMF_StateAdd(exportState, field, rc)
    !call ESMF_StateAdd(exportState, bundle, rc)
    print *, "Gridded Comp Init returning"

    rc = ESMF_SUCCESS

  end subroutine Init

  subroutine Run(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    print *, "Gridded Comp Run starting"
    ! call ESMF_StateGet(), etc to get fields, bundles, arrays
    !  from import state.

    call ESMF_ClockPrint(clock, options="currTime string")

    ! This is where the model specific computation goes.


    print *, "Gridded Comp Run returning"

    rc = ESMF_SUCCESS

  end subroutine Run

  subroutine Finalize(comp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    print *, "Gridded Comp Finalize starting"

    ! Add whatever code here needed

    print *, "Gridded Comp Finalize returning"

    rc = ESMF_SUCCESS

  end subroutine Finalize



end module
