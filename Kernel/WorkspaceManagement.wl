(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`WorkspaceManagement`"]



Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]
Needs["BradleyAshby`NotebookWorkspaces`Common`"]


$WorkspaceTaskUUID=None;
$FEPID=SystemInformation["FrontEnd", "ProcessID"];
$FEPersistenceLocation=SelectFirst[$PersistencePath,#["Type"]=="FrontEndSession"&];


$localworkspacerecord="NotebookWorkspaces/WorkspaceRecord";
$WorkspaceMetadata/:Set[$WorkspaceMetadata,value_]:=(
	LocalSymbol[$localworkspacerecord]=value;
	UpdatePalette[$WorkspaceMetadata];
	value)
$WorkspaceMetadata:=LocalSymbol[$localworkspacerecord]

WorkspaceMetadata[]:=(
	$WorkspaceMetadata/.{
		assoc_Association:>assoc,
		_LocalSymbol:>initializeMetadata[]
		}
	)

WorkspaceMetadata[workspace_String,___]/;!MemberQ[Keys[WorkspaceMetadata[]],workspace]:=Missing["WorkspaceNotInMetadata",workspace]
WorkspaceMetadata[workspace_String]:=Lookup[WorkspaceMetadata[],workspace]
WorkspaceMetadata[workspace_String,All]:=WorkspaceMetadata[workspace]
WorkspaceMetadata[workspace_String,value_String]:=With[
	{spacedata=WorkspaceMetadata[workspace]},
		Lookup[spacedata,value]
		]
WorkspaceMetadata[All,value_String]:=Lookup[value]/@WorkspaceMetadata[]


$localcurrentworkspace="NotebookWorkspaces/$CurrentWorkspace";
$CurrentWorkspace/:Set[$CurrentWorkspace,value_]:=(
	LocalSymbol[$localcurrentworkspace]=value;
	UpdatePalette[$CurrentWorkspace];
	value)
$CurrentWorkspace:=LocalSymbol[$localcurrentworkspace]

(*backwards compatibility:
	moving any stored $CurrentWorkspace to the new persistence method*)
With[{initval=InitializationValue[$CurrentWorkspace,"FrontEndSession"]},
	If[!MissingQ[initval],
		$CurrentWorkspace=initval;
		Remove[InitializationValue[$CurrentWorkspace,"FrontEndSession"]]
	]
]


initializeMetadata[]:=(
	$WorkspaceMetadata=<|
		$DefaultWorkspace-><|
			"FEPID"->None,
			"Event"->"InitializeNewDefaultWorkspace",
			"Timestamp"->Now,
			"SaveInformation"-><|"LastSaved"->"Never","SaveTrigger"->"None"|>
			|>
		|>)


WorkspaceSaveDirectory[]:= WorkspaceSaveDirectory[$CurrentWorkspace]
WorkspaceSaveDirectory[_Symbol]:=Failure["NoWorkspace",<|"MessageTemplate"->"No workspace selected"|>]
WorkspaceSaveDirectory[workspace_String]:= FileNameJoin[{$BaseSaveDirectory,workspace}]

WorkspaceNotebooksFile[workspace_String]:=FileNameJoin[{WorkspaceSaveDirectory[workspace],"notebooksrecord"}]


CreateWorkspace[workspace_String,log_String:"Workspace created"]/;!WorkspaceExistsQ[workspace]:=
	Enclose[
		Confirm[validateWorkspaceName[workspace]];
		ResourceFunction["EnsureDirectory"][WorkspaceSaveDirectory[workspace]];
		initializeWorkspaceNotebooksFile[workspace];
		initializeWorkspaceMetadata[workspace,log];
		
		Success["WorkspaceCreated",
			<|"MessageTemplate"->"Workspace `x` was created.",
			"MessageParameters"-><|"x"->workspace|>,
			"TimeStamp"->DateString[]|>]
		
	,"Expression"]

CreateWorkspace[workspace_String,log_String:"Workspace created"]/;WorkspaceExistsQ[workspace]:=
	Failure["WorkspaceExists",
		<|"MessageTemplate"->"The workspace \"`x`\" already exists.",
		"MessageParameters"-><|"x"->workspace|>,
		"TimeStamp"->DateString[]|>]


DeleteWorkspace[workspace_String]/;IntegerQ[activeWorkspacePID[workspace]]:=
	Failure["WorkspaceActive",<|
		"MessageTemplate"->"Workspace is currently active."
		|>]

DeleteWorkspace[workspace_String]:=(
		removeWorkspace[workspace];
		DeleteDirectory[WorkspaceSaveDirectory[workspace],DeleteContents->True];
	)


initializeWorkspaceNotebooksFile[workspace_String]:=
	Put[<|"Untitled"->{},"Saved"->{},"Timestamp"->Now,"Workspace"->workspace|>,WorkspaceNotebooksFile[workspace]]


SetWorkspace[workspace_String,log_String:"Workspace Set"]/;WorkspaceExistsQ[workspace]:=(
	(*if no workspace is active, initialize the general space first*)
	If[!$NotebookWorkspacesActiveQ,LoadGeneralWorkspace[]];

	UnsetWorkspace["Unset: "<>log];
	setWorkspace[workspace,log];
	$CurrentWorkspace
	)

setWorkspace[workspace_String,log_String:"Workspace Set"]:=(
	$CurrentWorkspace=workspace;
	$NotebookWorkspacesActiveQ=True;
	UpdatePalette[$CurrentWorkspace];
	setupForRestart[];
	
	LaunchWorkspaceTask[workspace,$FEPID];
	RecordWorkspaceMetadata[log];
	
	)


WorkspaceRestart::FESessionPersistenceMissing="FrontEndSession PersistenceLocation not found. Notebook Workspaces may not automatically restart after kernel quit.";
setupForRestart[]/;MissingQ[$FEPersistenceLocation]:=Message[WorkspaceRestart::FESessionPersistenceMissing]

setupForRestart[]:=(
	InitializationValue[WorkspacesRestarted,$FEPersistenceLocation,MergingFunction->ReleaseHold]=Hold[(
		Needs["BradleyAshby`NotebookWorkspaces`"->None];
		$CurrentWorkspace
		)]
	)


UnsetWorkspace[log_String:"UnsetWorkspace"]:=
	Module[{
			spaces=WorkspaceMetadata[],
			localspaces,
			tasks},
			
		localspaces=Select[spaces,#FEPID==$FEPID&];
		tasks=Cases[spaces,KeyValuePattern["TaskUUID"->uuid_String]/;taskExistQ[uuid]:>uuid];
		
		Quiet[TaskAbort[#];TaskRemove[#];&/@tasks];
		$CurrentWorkspace=None;
		UpdatePalette[$CurrentWorkspace];
		$WorkspaceTaskUUID=None;
		
		removeWorkspaceMetadata[#,log]&/@Keys[localspaces];
	]


initializeWorkspaceMetadata[workspace_String,log_String:"Workspace created"]:=
	Module[{workspaces=WorkspaceMetadata[]},
		
		AppendTo[workspaces,
			workspace-><|
				"FEPID"->None,
				"Timestamp"->Now,
				"TaskUUID"->None,
				"SaveInformation"-><|
					"LastSaved"->"Never",
					"SaveTrigger"->"None"|>,
				"Event"->log
				|>];
		
		$WorkspaceMetadata=workspaces;
			
		]


RecordWorkspaceMetadata[log_String:"NoneGiven"]:=
	RecordWorkspaceMetadata[$CurrentWorkspace,log]
	
RecordWorkspaceMetadata[workspace_String,log_String:"NoneGiven"]:=
	Module[{newlog},
		newlog=<|"Timestamp"->Now,"Event"->log|>;
		
		(*if this is for the current workspaces, log the FEPID and TaskUUID as well*)
		If[MemberQ[{$CurrentWorkspace,$GeneralWorkspace},workspace],
			AppendTo[newlog,<|
				"FEPID"->$FEPID,
				"TaskUUID"->$WorkspaceTaskUUID|>]
			];
			
		RecordWorkspaceMetadata[workspace,newlog]
	]

RecordWorkspaceMetadata[workspace_String,log_Association]:=
	Module[{
		workspaces=WorkspaceMetadata[],
		entry,defaultentry},

		(*automatically initialize if metadata is missing.
		this should be made more explicit instead of automatic*)
		defaultentry=<|
			"FEPID"->$FEPID,
			"Timestamp"->Now,
			"TaskUUID"->$WorkspaceTaskUUID,
			"SaveInformation"-><|
				"LastSaved"->"Never",
				"SaveTrigger"->"None"
				|>
			|>;
		entry=Lookup[workspaces,workspace,defaultentry];
		
		entry=Append[entry,log];
		entry=Append[entry,"Timestamp"->Now];
		
		AppendTo[workspaces,workspace->entry];
		
		$WorkspaceMetadata=workspaces;
		
	];


removeWorkspaceMetadata[workspace_String,log_String:"Metadata Cleared"]/;WorkspaceExistsQ[workspace]:=
	RecordWorkspaceMetadata[workspace,<|"FEPID"->None,"TaskUUID"->None,"Event"->log|>]


RebuildWorkspaceMetadata[]:=Module[
	{allworkspacefiles=FileNames["notebooksrecord",$BaseSaveDirectory,2],
	restoredata=<||>},

	Scan[
		With[{filedata=Get[#]},
			AppendTo[restoredata,
				Lookup[filedata,"Workspace"]-><|
					"SaveInformation"-><|"LastSaved"->Lookup[filedata,"Timestamp"]|>,
					"FEPID"->Lookup[filedata,"FEPID"],
					"TaskUUID"->None,
					"Event"->"RebuildWorkspaceMetadata",
					"Timestamp"->Now|>]
			]&,allworkspacefiles];

	$WorkspaceMetadata=Merge[{WorkspaceMetadata[],restoredata},First]

]


workspaceLastSaved[]:=With[{saveinfo=WorkspaceMetadata[All,"SaveInformation"]},
	Lookup["LastSaved"]/@saveinfo
	]

workspaceLastSaved[workspace_String]/;WorkspaceExistsQ[workspace]:=workspace/.workspaceLastSaved[]
workspaceLastSaved[workspace_String,"String"]:=DateString[workspaceLastSaved[workspace]]

workspaceLastSaved[_Symbol,___]:=""


activeWorkspacePID[workspace_String]/;WorkspaceExistsQ[workspace]:=Module[{workspaces,workspacemetadata,oldfepid,fepid,timestamp,oldfeprocess},
		
		workspaces=WorkspaceMetadata[];
		
		workspacemetadata=Lookup[workspaces,workspace];
		oldfepid=Lookup[workspacemetadata,"FEPID"];
		fepid=SystemInformation["FrontEnd","ProcessID"];
		timestamp=Lookup[workspacemetadata,"Timestamp"];
		oldfeprocess=SystemProcesses["PID"->oldfepid]/.{p_}:>p;
		
		Which[
		(* if the old fepid is None or references a process that's not running, its not active*)
		oldfeprocess/.{{}->True,_->False}, False,
		
		(* if the process is running but is newer than the last timestamp, its not actually active *)
		oldfeprocess["StartTime"]>timestamp, False,
		
		(* if the old process matches the current one *)
		oldfepid===fepid, fepid,
		
		(* if the old fepid points to a different active process *)
		And[!(oldfepid===fepid),StringContainsQ[oldfeprocess["Program"],"Mathematica"|"Wolfram",IgnoreCase->True]], oldfepid,
		
		(*Anything else, call it False and hope for the best *)
		True, False]
		
	]
	
activeWorkspacePID[workspace_String]/;!WorkspaceExistsQ[workspace]:=False


activeWorkspaceQ[workspace_]:=IntegerQ@activeWorkspacePID@workspace


SetAttributes[WithStatusUpdate,{HoldAll}]

WithStatusUpdate[status_String,expr_]:=WithCleanup[
	$WorkspaceStatus=status;UpdatePalette[$WorkspaceStatus],		
		expr,
	$WorkspaceStatus=None;UpdatePalette[$WorkspaceStatus]
	]


Options[SaveWorkspace]={"SaveAll"->False};

SaveWorkspace[allq_Symbol:False,None]:=SaveWorkspace["TaskUUIDMissing","SaveAll"->allq]

SaveWorkspace[allq_Symbol:False,log_String:"ManualSave"]:=SaveWorkspace[log,"SaveAll"->allq];

SaveWorkspace[log_String:"ManualSave",opts:OptionsPattern[]]/;WorkspaceExistsQ[$CurrentWorkspace]:=
	WithStatusUpdate["Saving...",
		If[!$NotebookWorkspacesActiveQ,SetWorkspace[$CurrentWorkspace]];
		
		With[{resp=SaveAndRecordNotebooks[OptionValue["SaveAll"],$CurrentWorkspace]},
			RecordWorkspaceMetadata[$CurrentWorkspace,<|"SaveInformation"-><|"LastSaved"->Now,"SaveTrigger"->log|>|>];
			RecordWorkspaceMetadata[$GeneralWorkspace,<|"SaveInformation"-><|"LastSaved"->Now,"SaveTrigger"->log,"ActiveWorkspace"->$CurrentWorkspace|>|>];
			resp
			]
		]

SaveWorkspace[log_String:"ManualSave",opts:OptionsPattern[]]/;!WorkspaceExistsQ[$CurrentWorkspace]:=
	WithStatusUpdate["Saving...",
		With[{newname=nameWorkspaceDialog[]},
			If[newname==$Canceled,
				Return[Failure["NoWorkspace",<|"MessageTemplate"->"No workspace selected. Load a workspace or use SaveWorkspaceAs"|>]]];
				
			SaveWorkspaceAs[newname]]
		
		]


SaveWorkspaceAs[newname_String]/;(newname==$CurrentWorkspace):=SaveWorkspace[]

SaveWorkspaceAs[newname_String]/;(WorkspaceExistsQ[newname]):=
	With[{resp=overwriteConfirm[newname]},
		If[TrueQ@resp,
			(*true*)
			DeleteWorkspace[newname];SaveWorkspaceAs[newname],
			(*false*)
			Abort[]
		]
	]

SaveWorkspaceAs[newname_String]/;(!WorkspaceExistsQ[newname]):=Enclose[
	Confirm[CreateWorkspace[newname]];
	SetWorkspace[newname];
	SaveWorkspace["SaveWorkspaceAs","SaveAll"->True];
	,"Expression"]

overwriteConfirm[newworkspace_String]:=
	ChoiceDialog["The workspace \""<>newworkspace<>"\" already exists. Overwrite?",
		{"Cancel"->False,"Overwrite"->True},Modal->True];


CloseWorkspace[]/;!WorkspaceExistsQ[$CurrentWorkspace]:=(
	UnsetWorkspace[];
	$NotebookWorkspacesActiveQ=False;
	)
(* Should the None case also give a message that no workspace is open?
	If we close all notebooks in this case, it needs a prompt. It's a destructive action *)

CloseWorkspace[]:=(
	closeWorkspace[$CurrentWorkspace];
	UnsetWorkspace[];
	$NotebookWorkspacesActiveQ=False;
	)

closeWorkspace[workspace_String]:=(
	TaskSuspend[$WorkspaceTaskUUID];
	SaveWorkspace[];
	closeNotebooks[workspace];
)


SwitchWorkspace[workspace_String]:=With[{
	closeonexit=CurrentValue[$FrontEnd,{PrivateNotebookOptions,"FinalWindowPrompt"}]},
		(* "FinalWindowPrompt"=False means the whole program is likely to close if on
			Windows or Mac machines. Temp set it to True so that won't happen.*)
		CurrentValue[$FrontEnd,{PrivateNotebookOptions,"FinalWindowPrompt"}]=True;
		
		closeWorkspace[$CurrentWorkspace];
		loadWorkspace[workspace];
		
		CurrentValue[$FrontEnd,{PrivateNotebookOptions,"FinalWindowPrompt"}]=closeonexit;
	]


LoadWorkspace[]:=LoadWorkspace[$DefaultWorkspace]

LoadWorkspace[workspace_String]/;WorkspaceExistsQ[workspace]:=(
	
	(* check edge cases before proceeding *)
	Switch[{activeWorkspacePID[workspace],$CurrentWorkspace},
		{False,_Symbol},loadWorkspace[workspace], (* nothing active, easy case *)
		{False,workspace}, unexpectedErrorState[workspace],(* error, ask to merge current and previous state OR cancel (and use SaveAs ) *)
		{False,_},SwitchWorkspace[workspace],
		{$FEPID,_Symbol}, unexpectedErrorState[workspace],(* unexpected error state *)
		{$FEPID,workspace}, SetWorkspace[workspace];SaveWorkspace[True,"Re-Loaded active workspace."], 
		{$FEPID,_},SwitchWorkspace[workspace], (* strange state, but switching should smooth things out *)
		{_Integer,_}, workspaceActive[workspace],
		_, unexpectedErrorState[workspace]
		]
)

LoadWorkspace[workspace_String]/;!WorkspaceExistsQ[workspace]:=Failure["WorkspaceDNE",<|"MessageTemplate"->"The named workspace does not exist."|>]

loadWorkspace[workspace_String]:=WithStatusUpdate["Workspace loading...",		
		SetWorkspace[workspace];
		ReopenNotebooks[workspace];
		SaveWorkspace[True,"SaveOnLoad"];
	]


RemoveWorkspace[workspace_String]:=Module[{FEPID=activeWorkspacePID[workspace]},
	Switch[FEPID,
		$FEPID,(UnsetWorkspace["Removing workspace"];removeWorkspace[workspace]),
		_Integer,workspaceActive[workspace],
		False,removeWorkspace[workspace]]
	]

removeWorkspace[workspace_String]:=(
	$WorkspaceMetadata=KeyDrop[WorkspaceMetadata[],workspace]
	)


CleanWorkspace[]/;WorkspaceExistsQ[$CurrentWorkspace]:=
	Module[{dir=WorkspaceSaveDirectory[$CurrentWorkspace],files,choice},
		
		choice=ChoiceDialog[
			Column[{
				"Delete all notebook and package files in the directory?",
				Spacer[1],
				dir,
				Button["Inspect directory",SystemOpen[dir],ImageSize->Automatic]
			}],
			
			{"Cancel"->False,"Delete"->True},
			
			Modal->True,WindowTitle->"Clean Workspace"
		];
		
		files=Select[FileNames[All,dir],FileFormat[#]/.{"NB"|"WL"->True,_->False}&];
		
		If[choice,
			DeleteFile[files],
			Abort[]
		];
		
		SaveWorkspace[True,"CleanWorkspace[]"]
				
	]


AddNotebookToWorkspace[workspace_String,rest___]/;!WorkspaceExistsQ[workspace]:=
	Enclose[
		Confirm[CreateWorkspace[workspace]];
		AddNotebookToWorkspace[workspace,rest]
	,"Expression"]

AddNotebookToWorkspace[workspace_String]:=AddNotebookToWorkspace[workspace,EvaluationNotebook[]]

AddNotebookToWorkspace[workspace_String,notebook_NotebookObject]:=
	With[{nbtitle=Information[notebook,"WindowTitle"]},
	
		SaveNotebook[notebook,workspace];
		RecordNotebookToWorkspace[notebook,workspace];
		RecordWorkspaceMetadata[workspace,nbtitle<>" added to workspace"];
		If[workspace==$GeneralWorkspace,
			AddNotebookToGeneralList[notebook]];
		
		Success["NotebookAdded",<|
			"MessageTemplate":>"`notebook` has been added to `workspace`",
			"MessageParameters"-><|
				"notebook"->nbtitle,
				"workspace"->workspace|>,
			"TimeStamp"->DateString[]
			|>]
	]


validateWorkspaceName[workspacename_String]/;WorkspaceExistsQ[workspacename]:=
	Failure["WorkspaceExists",
		<|"MessageTemplate"->"The workspace \"`x`\" already exists.",
		"MessageParameters"-><|"x"->workspacename|>,
		"TimeStamp"->DateString[]|>]

validateWorkspaceName[workspacename_String]:=Enclose[
	Confirm[If[!StringFreeQ[workspacename,Except[{WordCharacter,WhitespaceCharacter}]],Failure["BadName",<|"MessageTemplate"->"Workspace names can only contain letters, numbers, and spaces."|>]]];
	workspacename
	,"Expression"]


workspaceActive[workspace_]:=Return[Failure["AlreadyActive",<|"MessageTemplate"->"Workspace is already active on another process."|>]]


(* unexpected state, ask user to merge current with saved state *)
unexpectedErrorState[workspace_]:=With[
	{choice=unexpectedErrorStateDialog[workspace]},
		Switch[choice,
			"Merge",loadWorkspace[workspace],
			"Set",SetWorkspace[workspace],
			"Clear",(UnsetWorkspace[];closeNotebooks[];LoadWorkspace[workspace]),
			"Cancel",Abort[],
			_,$Failed
		]	
	];

unexpectedErrorStateDialog[workspace_]:=
	ChoiceDialog[
		Column[{
			"Workspaces encountered an unexpected state while trying to load \""<>workspace<>"\". Please choose a resolution below or Cancel to resolve this manually.",
			Spacer[1],
			Grid[{
				{"Merge:","Load the saved state from \""<>workspace<>"\" without closing current notebooks."},
				{"Set:","Set \""<>workspace<>"\" to be the current set of notebooks."},
				{"Clear:","Close current notebooks without saving and load the saved state of \""<>workspace<>"\"."}
			},Alignment->Left]
		},Alignment->Left],
		{"Merge"->"Merge","Set"->"Set","Clear"->"Clear","Cancel"->"Cancel"},Modal->True
	];


taskExistQ[uuid_String]:=Cases[Tasks[],
	TaskObject@KeyValuePattern["TaskUUID"->uuid]]/.{{}->False,_->True}

taskExistQ[___]:=False


LaunchWorkspaceTask[args___]:=With[{task=launchWorkspaceTask[args]}, $WorkspaceTaskUUID= task["TaskUUID"]];


launchWorkspaceTask[workspace_String,fepid_Integer]:=With[{space=workspace,pid=fepid},
	SessionSubmit[
		ScheduledTask[
			SaveWorkspaceTask[workspace,fepid],
			$SaveFrequency]
	]
]


SaveWorkspaceTask[workspace_String,fepid_Integer]:=With[{currentspace={$CurrentWorkspace,SystemInformation["FrontEnd","ProcessID"]}},
	If[currentspace!={workspace,fepid},
		Return[Failure["WorkspacePIDMismatch",<|"MessageTemplate"->"Workspace and PID no longer match current values."|>]],
		SaveWorkspace[False,$WorkspaceTaskUUID]
	]
]


taskExistQ[uuid_String]:=Cases[Tasks[],TaskObject@KeyValuePattern["TaskUUID"->uuid]]/.{{}->False,_->True}


$NotebookWorkspacesActiveQ=TrueQ[activeWorkspacePID[$CurrentWorkspace]===$FEPID];


RestartWorkspaces[]/;$NotebookWorkspacesActiveQ:=SetWorkspace[$CurrentWorkspace,"Reset after Quit"]

RestartWorkspaces[]/;!$NotebookWorkspacesActiveQ:=($CurrentWorkspace=None)


SessionSubmit[ScheduledTask[
		With[{initsloaded=TrueQ[System`Private`$InitsLoaded]},
			If[initsloaded,
				RestartWorkspaces[];
				TaskRemove[$CurrentTask]]],
	Quantity[1,"Seconds"]]]


End[];

EndPackage[]
