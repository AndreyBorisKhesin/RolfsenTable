BeginPackage["Profile`"]

ProfileOff[] := (
  $Profiling = False;
  Attributes[Profile]={HoldFirst};
  Profile[label_,expr_] := expr
)

ProfileOff[]

ProfileOn[] := (
  $Profiling=True;
  Attributes[Profile]={HoldRest};
  Profile[label_,expr_] := (
    Block[
      {
        PreviousLabel=CurrentLabel,
        CurrentLabel=label,
        EntryTime=TimeUsed[],
        AdjustedEntryTime
      },
      AdjustedEntryTime = EntryTime;
      CallingHistory += Called[PreviousLabel,CurrentLabel];
      value=expr;
      TotalTime += (TimeSpent=(TimeReading=TimeUsed[])-EntryTime) *
        TimeUnder[PreviousLabel,CurrentLabel];
      SelfTime += (TimeReading-AdjustedEntryTime) *
        TimeUnder[PreviousLabel,CurrentLabel];
    ];
    AdjustedEntryTime += TimeSpent;
    value
  )
)

BeginProfile[] := BeginProfile[ProfileRoot]
BeginProfile[root_] := (
  ProfileOn[];
  CallingHistory = TotalTime = SelfTime = 0;
  RootEntryTime = AdjustedEntryTime = TimeUsed[];
  $ProfileRoot = CurrentLabel = root
)

EndProfile[] := (
  ProfileOff[];
  ProfileData[$ProfileRoot,CallingHistory,TotalTime,SelfTime]
)

$CurrentProfile := ProfileData[$ProfileRoot,CallingHistory,TotalTime,SelfTime]

TimeIn[label_] := TimeIn[label,$CurrentProfile]
TimeIn[label_,ProfileData[_,_,_,st_]] := st /. {
  TimeUnder[_,label] -> 1,
  TimeUnder[_,_] -> 0
}

ProfileLabels[] := ProfileLabels[$CurrentProfile]
ProfileLabels[pd_ProfileData] := Block[
  {out={}},
  pd[[2]] /. Called[lbls__] :> (out=Union[out,{lbls}];);
  Reverse[Last /@ Sort[{TimeIn[#,pd],#}& /@ out]]
]

PrintProfile[pd_ProfileData] :=
  PrintProfile[#,pd]& /@ ProfileLabels[pd]
PrintProfile[]:=PrintProfile[$CurrentProfile]
PrintProfile[label_] := PrintProfile[label,$CurrentProfile]
PrintProfile[label_,ProfileData[pr_,ch_,tt_,st_]] := Block[
  {labelist={},z,l1,l2},
  l1=Floor[N[Log[10, ch /. _Called -> 1]]];
  l2=3+Floor[N[Log[10, tt /. _TimeUnder -> 1]]];
  Print[label,": called ",
    ch /. {Called[_,label] -> 1, Called[_,_] -> 0},
    " times, time in ",
    TimeIn[label],
    "/",
    tt /. {TimeUnder[_,label] -> 1, TimeUnder[_,_] -> 0}
  ];
  ch /.  Called[lbl_,label] :> (labelist=Union[labelist,{lbl}];);
  If[Length[labelist]>0,
    Print["  Parents:"];
    Do[
      Print[StringForm["    (``) ``/`` under ``", 
        PaddedForm[Coefficient[ch,Called[labelist[[z]],label]],l1], 
        PaddedForm[Coefficient[st,TimeUnder[labelist[[z]],label]],{l2,3}],  
        PaddedForm[Coefficient[tt,TimeUnder[labelist[[z]],label]],{l2,3}], 
        labelist[[z]]
      ]],
      {z,1,Length[labelist]}
    ]
  ];
  labelist={};
  ch /.  Called[label,lbl_] :> (labelist=Union[labelist,{lbl}];);
  If[Length[labelist]>0,
    Print["  Children:"];
    Do[
      Print[StringForm["    (``) ``/`` above ``",
        PaddedForm[Coefficient[ch,Called[label,labelist[[z]]]],l1],
        PaddedForm[Coefficient[st,TimeUnder[label,labelist[[z]]]],{l2,3}],
        PaddedForm[Coefficient[tt,TimeUnder[label,labelist[[z]]]],{l2,3}],
        labelist[[z]]
      ]], 
      {z,1,Length[labelist]}
    ]
  ];
  Print[""];
]

EndPackage[]
