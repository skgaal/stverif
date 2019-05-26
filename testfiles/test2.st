// PosForceCtrl action
ACTION PosForceCtrl :
  IF enableCtrlValves.out THEN
    CtrlLEFT    := CtrlLEFT  + 3.0;
  ELSE
    CtrlRIGHT   := CtrlRIGHT + 3.0;
  END_IF
END_ACTION
