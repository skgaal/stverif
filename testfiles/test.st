
PROGRAM name_this_program_pls

	// Initialize ControlValues

	FOR n := 1 TO 16 DO
	    k := (45.0 * (k + I_TO_R(n))) / 1000.0;
	END_FOR

	CtrlLEFT  := k;
	CtrlRIGHT := k;

	// Case from the ControlValue with Controller logic action call
  CASE k OF
      1,3,7   :   CtrlLEFT  := a;
      4,6     :   CtrlRIGHT := b;
      ELSE        PosForceCtrl;
  END_CASE

	// Map controller output to hardware
	IF enableCtrlValves.out THEN
	    RightValve.Value    := CtrlLEFT;
	    LeftValve.Value        := CtrlRIGHT;
	ELSE
      ValveCtrl := 0;
  END_IF
END_PROGRAM
