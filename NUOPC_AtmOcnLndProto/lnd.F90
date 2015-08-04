module LND

    !-----------------------------------------------------------------------------
    ! LND Component.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Model, &
        model_routine_SS    => SetServices, &
        model_label_Advance => label_Advance, &
        model_label_SetClock => label_SetClock
  
    implicit none
  
    private
  
    public SetServices
  
!-----------------------------------------------------------------------------
contains
    !-----------------------------------------------------------------------------
  
    subroutine SetServices(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc
    
        rc = ESMF_SUCCESS
    
        ! the NUOPC model component will register the generic methods
        call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! set entry point for methods that require specific implementation
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! attach specializing method(s)
        call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
            specRoutine=ModelAdvance, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call NUOPC_CompSpecialize(model, specLabel=model_label_SetClock, &
            specRoutine=SetClock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
    end subroutine
  
    !-----------------------------------------------------------------------------

    subroutine InitializeP1(model, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc
    
        rc = ESMF_SUCCESS

        call NUOPC_FieldDictionaryAddEntry("sensible_heat_flux", "W m-2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        ! exportable field: sensible_heat_flux
        call NUOPC_StateAdvertiseField(exportState, &
            StandardName="sensible_heat_flux", name="qheat", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out


    end subroutine
  
    !-----------------------------------------------------------------------------

    subroutine InitializeP2(model, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: model
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc
    
        ! local variables
        type(ESMF_Field)        :: field
        type(ESMF_Grid)         :: gridIn
        type(ESMF_Grid)         :: gridOut
    
        rc = ESMF_SUCCESS
    
        ! create a Grid object for Fields
        gridIn = NUOPC_GridCreateSimpleXY(10._ESMF_KIND_R8, 20._ESMF_KIND_R8, &
            100._ESMF_KIND_R8, 200._ESMF_KIND_R8, 10, 100, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        gridOut = gridIn ! for now out same as in

        ! exportable field: air_pressure_at_sea_level
        field = ESMF_FieldCreate(name="qheat", grid=gridOut, &
            typekind=ESMF_TYPEKIND_R8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out


    end subroutine
  
    !-----------------------------------------------------------------------------

    subroutine ModelAdvance(model, rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc
    
        ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_State)              :: importState, exportState

        rc = ESMF_SUCCESS
    
        ! query the Component for its clock, importState and exportState
        call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
            exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
        ! Because of the way that the internal Clock was set by default,
        ! its timeStep is equal to the parent timeStep. As a consequence the
        ! currTime + timeStep is equal to the stopTime of the internal Clock
        ! for this call of the ModelAdvance() routine.
    
        call NUOPC_ClockPrintCurrTime(clock, &
            "------>Advancing LND from: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        call NUOPC_ClockPrintStopTime(clock, &
            "--------------------------------> to: ", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine

    subroutine SetClock(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_TimeInterval)       :: stabilityTimeStep

        rc = ESMF_SUCCESS
    
        ! query the Component for its clock, importState and exportState
        call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      
        ! initialize internal clock
        ! here: parent Clock and stability timeStep determine actual model timeStep
        call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    
        call NUOPC_CompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

    end subroutine


end module
