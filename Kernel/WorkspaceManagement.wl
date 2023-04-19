(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`WorkspaceManagement`"]


$WorkspaceMetadata
WorkspaceSaveDirectory
WorkspaceNotebooksFile
workspaceExistQ
$FEPID
activeWorkspacePID
validateWorkspaceName
workspaceLastSaved

Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]
Needs["BradleyAshby`NotebookWorkspaces`Configuration`"]
Needs["BradleyAshby`NotebookWorkspaces`GeneralWorkspace`"]
Needs["BradleyAshby`NotebookWorkspaces`SaveAndRecordNotebooks`"]
Needs["BradleyAshby`NotebookWorkspaces`Palette`"]


$WorkspaceTaskUUID=None;
$FEPID=SystemInformation["FrontEnd", "ProcessID"];



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

WorkspaceMetadata[workspace_String]:=Lookup[WorkspaceMetadata[],workspace]
WorkspaceMetadata[workspace_String,All]:=WorkspaceMetadata[workspace]
WorkspaceMetadata[workspace_String,value_String]/;workspaceExistQ[workspace]:=With[
	{spacedata=WorkspaceMetadata[workspace]},
		Lookup[spacedata,value]
		]
WorkspaceMetadata[All,value_String]:=Lookup[value]/@WorkspaceMetadata[]


initializeMetadata[]:=(
	$WorkspaceMetadata=<|
		$DefaultWorkspace-><|
			"FEPID"->None,
			"Event"->"InitializeNewDefaultWorkspace",
			"Timestamp"->Now,
			"SaveInformation"-><|"LastSaved"->"Never","SaveTrigger"->"None"|>
			|>
		|>)


WorkspaceSaveDirectory[]:= WorkspaceSaveDirectory[$CurrentWorkspace];
WorkspaceSaveDirectory[_Symbol]:=Failure["NoWorkspace",<|"MessageTemplate"->"No workspace selected"|>];
WorkspaceSaveDirectory[workspace_String]:= FileNameJoin[{$BaseSaveDirectory,workspace}];

WorkspaceNotebooksFile[workspace_String]:=FileNameJoin[{WorkspaceSaveDirectory[workspace],"notebooksrecord"}]


SetWorkspace[workspace_String,log_String:"Workspace Set"]:=Enclose[
	Confirm[validateWorkspaceName[workspace]];
	UnsetWorkspace["Unset:"<>log];
	setWorkspace[workspace,log];
	$CurrentWorkspace
	,"Expression"]

setWorkspace[workspace_String,log_String:"Workspace Set"]:=(
	InitializationValue[$CurrentWorkspace,"FrontEndSession"]=$CurrentWorkspace=workspace;
	UpdatePalette[$CurrentWorkspace];
	setupForRestart[];
	ResourceFunction["EnsureDirectory"][WorkspaceSaveDirectory[workspace]];
	
	initializeGeneralWorkspace[];
	
	LaunchWorkspaceTask[workspace,$FEPID];
	recordWorkspaceMetadata[log];
	
	)


setupForRestart[]:=(
	InitializationValue[WorkspacesRestarted,"FrontEndSession",MergingFunction->ReleaseHold]=Hold[(
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
		InitializationValue[$CurrentWorkspace,"FrontEndSession"]=$CurrentWorkspace=None;
		UpdatePalette[$CurrentWorkspace];
		$WorkspaceTaskUUID=None;
		
		removeWorkspaceMetadata[#,log]&/@Keys[localspaces];
	]


recordWorkspaceMetadata[log_String:"NoneGiven"]:=
	recordWorkspaceMetadata[$CurrentWorkspace,
		<|
		"FEPID"->$FEPID,
		"Timestamp"->Now,
		"TaskUUID"->$WorkspaceTaskUUID,
		"Event"->log
		|>
	]

recordWorkspaceMetadata[workspace_String,log_Association]:=
	Module[{
		workspaces=WorkspaceMetadata[],
		defaultentry=<|"FEPID"->$FEPID,"Timestamp"->Now,"TaskUUID"->$WorkspaceTaskUUID,"SaveInformation"-><|"LastSaved"->"Never","SaveTrigger"->"None"|>|>,
		entry
		},
		
		entry=Lookup[workspaces,workspace]/._Missing->defaultentry;
		entry=Append[entry,log];
		entry=Append[entry,"Timestamp"->Now];
		
		AppendTo[workspaces,workspace->entry];
		
		$WorkspaceMetadata=workspaces;
		
	];


removeWorkspaceMetadata[workspace_String]/;workspaceExistQ[workspace]:=
	recordWorkspaceMetadata[workspace,<|"FEPID"->None,"TaskUUID"->None,"Event"->"Metadata Cleared"|>]

removeWorkspaceMetadata[workspace_String,log_String]/;workspaceExistQ[workspace]:=
	recordWorkspaceMetadata[workspace,<|"FEPID"->None,"TaskUUID"->None,"Event"->log|>]


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

	$WorkspaceMetadata=Merge[{WorkspaceMetadata[],KeyDrop[restoredata,$GeneralWorkspace]},First]

]


workspaceExistQ[_Symbol]:=False;
workspaceExistQ[workspace_String]:=FileExistsQ[WorkspaceNotebooksFile[workspace]];


workspaceLastSaved[]:=With[{saveinfo=WorkspaceMetadata[All,"SaveInformation"]},
	Lookup["LastSaved"]/@saveinfo
	]

workspaceLastSaved[workspace_String]/;workspaceExistQ[workspace]:=workspace/.workspaceLastSaved[]
workspaceLastSaved[workspace_String,"String"]:=DateString[workspaceLastSaved[workspace]]

workspaceLastSaved[_Symbol,___]:=""


activeWorkspacePID[workspace_String]/;workspaceExistQ[workspace]:=Module[{workspaces,workspacemetadata,oldfepid,fepid,timestamp,oldfeprocess},
		
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
		And[!(oldfepid===fepid),StringContainsQ[oldfeprocess,"Mathematica"|"Wolfram",IgnoreCase->True]], oldfepid,
		
		(*Anything else, call it False and hope for the best *)
		True, False]
		
	]
	
activeWorkspacePID[workspace_String]/;!workspaceExistQ[workspace]:=False


activeWorkspaceQ[workspace_]:=IntegerQ@activeWorkspacePID@workspace


Options[SaveWorkspace]={"SaveAll"->False};

SaveWorkspace[allq_Symbol:False,None]:=SaveWorkspace["TaskUUIDMissing","SaveAll"->allq]

SaveWorkspace[allq_Symbol:False,log_String:"ManualSave"]:=SaveWorkspace[log,"SaveAll"->allq];

SaveWorkspace[log_String:"ManualSave",opts:OptionsPattern[]]:=Module[{resp},
	If[workspaceExistQ[$CurrentWorkspace],
		resp=SaveAndRecordNotebooks[OptionValue["SaveAll"],$CurrentWorkspace];
		recordWorkspaceMetadata[$CurrentWorkspace,<|"SaveInformation"-><|"LastSaved"->Now,"SaveTrigger"->log|>|>];
		
		SelectFirst[resp,#["Workspace"]==$CurrentWorkspace&],
		
		Failure["NoWorkspace",<|"MessageTemplate"->"No workspace selected. Load a workspace or use SaveWorkspaceAs"|>]
		]
	]


(* set the space but don't reopen any notebooks *)
SaveWorkspaceAs[newname_String]:=Enclose[
	SaveWorkspaceAs[$CurrentWorkspace,Confirm[validateWorkspaceName[newname]]];
	,"Expression"]
	
SaveWorkspaceAs[workspace_,newname_String]/;(workspace==$CurrentWorkspace):=
	saveWorkspaceAs0[workspace,newname]
(*SaveWorkspaceAs[workspace_String,newname_String]/;workspaceExistQ[workspace]:=
	not the current workspace, but exists:	not implemented;
	copyWorkspace[workspace,newname]*)

saveWorkspaceAs0[workspace_,newname_String]/;(!workspaceExistQ[newname]):=
	saveWorkspaceAs[workspace,newname];
	
saveWorkspaceAs0[workspace_,newname_String]/;(workspaceExistQ[newname]):=
	With[{resp=overwriteConfirm[newname]},
		If[TrueQ@resp,
			saveWorkspaceAs[workspace,newname],
			Abort[]
		]	
]

overwriteConfirm[newworkspace_String]:=
	ChoiceDialog["The workspace \""<>newworkspace<>"\" already exists. Overwrite?",
		{"Cancel"->False,"Overwrite"->True},Modal->True];

saveWorkspaceAs[workspace_,newname_String]:=
	(
	SetWorkspace[newname];
	SaveWorkspace[];
	)

(*copyWorkspace[workspace_String,newname_String]/;workspaceExistQ[workspace]:=;*)
(*copy directory, etc. don't forget to confirm overwrite*)


CloseWorkspace[]:=closeWorkspace[$CurrentWorkspace]

closeWorkspace[_Symbol]:=UnsetWorkspace[]; 
(* Should the None case also give a message that no workspace is open?
	If we close all notebooks in this case, it needs a prompt. It's a destructive action *)

closeWorkspace[workspace_String]:=(
	TaskSuspend[$WorkspaceTaskUUID];
	TaskAbort[$WorkspaceTaskUUID];
	SaveWorkspace[];
	UnsetWorkspace[];
	closeNotebooks[workspace];
)


SwitchWorkspace[workspace_String]:=With[{
	closeonexit=CurrentValue[$FrontEnd,{PrivateNotebookOptions,"FinalWindowPrompt"}]},
		(* "FinalWindowPrompt"=False means the whole program is likely to close if on
			Windows or Mac machines. Temp set it to True so that won't happen.*)
		CurrentValue[$FrontEnd,{PrivateNotebookOptions,"FinalWindowPrompt"}]=True;
		
		CloseWorkspace[];
		loadWorkspace[workspace];
		
		CurrentValue[$FrontEnd,{PrivateNotebookOptions,"FinalWindowPrompt"}]=closeonexit;
	]


LoadWorkspace[]:=LoadWorkspace[$DefaultWorkspace]

LoadWorkspace[workspace_String]/;workspaceExistQ[workspace]:=(
	
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

LoadWorkspace[workspace_String]/;!workspaceExistQ[workspace]:=Failure["WorkspaceDNE",<|"MessageTemplate"->"The named workspace does not exist."|>]

loadWorkspace[workspace_String]:=(		
		SetWorkspace[workspace];
		ReopenNotebooks[workspace];
		SaveWorkspace[True,"SaveOnLoad"];
)


RemoveWorkspace[workspace_String]:=Module[{FEPID=activeWorkspacePID[workspace]},
	Switch[FEPID,
		$FEPID,(UnsetWorkspace["Removing workspace"];removeWorkspace[workspace]),
		_Integer,workspaceActive[workspace],
		False,removeWorkspace[workspace]]
]

removeWorkspace[workspace_String]:=(
	$WorkspaceMetadata=KeyDrop[WorkspaceMetadata[],workspace]
)


CleanWorkspace[]:=
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
				
	]/;workspaceExistQ[$CurrentWorkspace]


validateWorkspaceName[workspacename_String]:=Enclose[
	Confirm[If[workspacename=="GeneralWorkspace",Failure["NameReserved",<|"MessageTemplate"->"The name \"GeneralWorkspace\" is reserved. Select another."|>]]];
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


feRestartedQ:=!ValueQ@$CurrentWorkspace;


RestartWorkspaces[]/;!feRestartedQ:=With[{
	iscurrent=TrueQ[WorkspaceMetadata[$CurrentWorkspace,"FEPID"]==$FEPID]
	},

	If[iscurrent,
		setWorkspace[$CurrentWorkspace,"Reset after Quit"]
		]
	]

RestartWorkspaces[]/;feRestartedQ:=Nothing;


SessionSubmit[ScheduledTask[RestartWorkspaces[],{Quantity[1,"Seconds"],1}]]


End[];

EndPackage[]
