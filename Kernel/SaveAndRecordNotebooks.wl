(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`SaveAndRecordNotebooks`"]

SaveNotebook
RecordNotebookToWorkspace
RecordWorkspaceNotebooks
SaveAndRecordNotebooks
ReopenNotebooks
closeNotebooks
$ExcludedNotebooks


Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]
Needs["BradleyAshby`NotebookWorkspaces`Configuration`"]
Needs["BradleyAshby`NotebookWorkspaces`WorkspaceManagement`"]
Needs["BradleyAshby`NotebookWorkspaces`GeneralWorkspace`"]
Needs["BradleyAshby`NotebookWorkspaces`Palette`"]


$localexcludednotebooks="NotebookWorkspaces/ExcludedNotebooks";
$ExcludedNotebooks/:Set[$ExcludedNotebooks,value_]:=(
	LocalSymbol[$localexcludednotebooks]=value;
	UpdateDynamicsUsing[$ExcludedNotebooks];
	value)
$ExcludedNotebooks:=Replace[LocalSymbol[$localexcludednotebooks],Except[_List]->{"Messages"}]


ExcludedNotebooks[]:=$ExcludedNotebooks


AddToExcludedNotebooks[list_List]:=(
	AddToExcludedNotebooks/@list;
	ExcludedNotebooks[])

AddToExcludedNotebooks[nb_NotebookObject]:=With[{nbtitle=Information[nb,"WindowTitle"]},
	AddToExcludedNotebooks[nbtitle]
	]

AddToExcludedNotebooks[nbtitle_String]:=(
	$ExcludedNotebooks=DeleteDuplicates[Append[$ExcludedNotebooks,nbtitle]]
	)

(*AddToExcludedNotebooks[nb_File]:=()*)


RemoveFromExcludedNotebooks[list_List]:=(
	RemoveFromExcludedNotebooks/@list;
	ExcludedNotebooks[])

RemoveFromExcludedNotebooks[nb_NotebookObject]:=With[{nbtitle=Information[nb,"WindowTitle"]},
	RemoveFromExcludedNotebooks[nbtitle]
	]

RemoveFromExcludedNotebooks[nb_String]:=With[{excluded=$ExcludedNotebooks},
	$ExcludedNotebooks=DeleteCases[excluded,nb]
	]


	(* Notebooks with an existing save location *)
SaveNotebook[nb_NotebookObject,workspace_String]/;FileExistsQ[Quiet[NotebookFileName[nb]]]:=(
	NotebookSave[nb];
	)
	
		(* Untitled/unsaved notebooks *)
SaveNotebook[nb_NotebookObject,workspace_String]:=
	With[{filepath=notebookFile[nb,workspace]},
		Export[filepath,nb,OverwriteTarget->True]
	]


(* Record a single notebook into a workspace *)
RecordNotebookToWorkspace[nb_NotebookObject,workspace_String]/;workspaceExistQ[workspace]:=
	Module[{key,filepath=notebookFile[nb,workspace],oldrecord,newrecord},
		
		key=Switch[FileExistsQ[Quiet[NotebookFileName[nb]]],
			True,"Saved",
			False,"Untitled"];
		
		oldrecord=Get@WorkspaceNotebooksFile[workspace];
		newrecord=ResourceFunction["AppendAt"][oldrecord,filepath,{key}];
		
		Put[newrecord,WorkspaceNotebooksFile[workspace]];
		
		newrecord
	]


RecordWorkspaceNotebooks[nblist:{_NotebookObject...},workspace_String]:=
	Module[{groupednotebooks,untitled,saved,workspacerecord},
		
		groupednotebooks=GroupBy[nblist,
			(FileExistsQ[Quiet[NotebookFileName[#]]]&)->
				(notebookFile[#,workspace]&)];
		untitled=Lookup[groupednotebooks,False,{}];
		saved=Lookup[groupednotebooks,True,{}];
		
		workspacerecord=<|
			"Untitled"->untitled,
			"Saved"->saved,
			"Timestamp"->Now,
			"Workspace"->workspace,
			"FEPID"->$FEPID|>;
		
		Put[workspacerecord,WorkspaceNotebooksFile[workspace]];
		
		(*if this is the general space, remove uuids for closed notebooks*)
		If[workspace==$GeneralWorkspace,
			PruneGeneralNotebookUUIDs[]
		];
		
		workspacerecord			
	]


notebookFile[nb_NotebookObject,workspace_String]:=
	With[{filepath=Quiet[NotebookFileName[nb]]},
		Switch[FileExistsQ[filepath],
			True,filepath,
			False,unsavedNotebookFile[nb,workspace]
		]
	]

unsavedNotebookFile[nb_NotebookObject,workspace_String]:=
	Module[{nbtitle=Quiet[Information[nb,"WindowTitle"]],filepath,differentiator=""},
		
		nbtitle=nbtitle/._Missing->Information[nb,"ExpressionUUID"];
		(*the differentiator helps prevents overwriting if a notebook is being added when the space is not current*)
		If[workspace!=$CurrentWorkspace,
			differentiator="_"<>Hash[Information[nb,"ExpressionUUID"],"Expression","HexString"]];
		
		filepath=FileNameJoin[{
			WorkspaceSaveDirectory[workspace],
			ResourceFunction["SlugifyString"][nbtitle<>differentiator,"ForceLowerCase"->False]<>".nb"}];
		
		filepath		
	]


systemNotebookQ[nb_NotebookObject]:=TrueQ@With[{dir=Quiet@NotebookDirectory@nb},
	And[
		StringQ@dir,
		!StringFreeQ[dir,
			Alternatives[
				$BaseDirectory,
				$InstallationDirectory,
				$UserBasePacletsDirectory,
				PacletManager`$SystemDocumentationDirectory/.None->$BaseDirectory

			]
		]
	]
];


recordableNotebooks[workspace_String]/;(workspace==$GeneralWorkspace):=GeneralNotebooks[]

recordableNotebooks[HoldPattern[workspace_:$CurrentWorkspace]]:=
	With[
		{excluded=$ExcludedNotebooks},
	
		Select[Complement[Notebooks[],GeneralNotebooks[]],
			Module[{title,type,uuid},
				{title,type,uuid}=Information[#,
					{"WindowTitle","DocumentType","ExpressionUUID"}];	
				
				And[
					Not[MemberQ[excluded,title]],
					Not[type=="Help"],
					TrueQ[CurrentValue[#,Visible]],
					Not[systemNotebookQ[#]]
			        ]
			]&
		]
	]


SaveAndRecordNotebooks::duplicates="Duplicate notebooks saved. Not all will be recovered.";
SaveAndRecordNotebooks::opennotebooks="Too few open notebooks. Notebooks saved but not recorded.";

SaveAndRecordNotebooks[allq_Symbol:False]/;BooleanQ[allq]:=SaveAndRecordNotebooks[allq,$CurrentWorkspace]
SaveAndRecordNotebooks[allq_Symbol:False,workspace_String]/;!workspaceExistQ[workspace]:=Failure["NoWorkspace",<|"MessageTemplate"->"No workspace loaded"|>]

SaveAndRecordNotebooks[allq_Symbol:False,workspace_String]:=
	With[{
		workspacelastsaved=Lookup[WorkspaceMetadata[workspace,"SaveInformation"],"LastSaved"]},
		
		saveAndRecordNotebooks[allq,$GeneralWorkspace,workspacelastsaved];
		saveAndRecordNotebooks[allq,workspace,workspacelastsaved]		
		]

saveAndRecordNotebooks[allq_Symbol:False,workspace_String,workspacelastsaved_]:=
	Module[{recordablenotebooks,saveablenotebooks,opennotebooks},

		recordablenotebooks=recordableNotebooks[workspace];
		If[Or[allq,MissingQ[workspacelastsaved]],
			saveablenotebooks=recordablenotebooks,
			
			saveablenotebooks=Select[recordablenotebooks,
				Module[{modifiedq,modtime},
				{modifiedq,modtime}=Information[#,
					{"ModifiedInMemory","MemoryModificationTime"}];	
				
				And[		
					modifiedq,
					workspacelastsaved<modtime,
					TrueQ[AbsoluteCurrentValue[#,Saveable]]
			        ]
				]&]
			];

		SaveNotebook[#,workspace]&/@saveablenotebooks;
		opennotebooks=RecordWorkspaceNotebooks[recordablenotebooks,workspace];

		If[!DuplicateFreeQ[Flatten@Lookup[opennotebooks,{"Untitled","Saved"},{}]],
			Message[SaveAndRecordNotebooks::duplicates]
			];
		
		KeyDrop[opennotebooks,"FEPID"]

]


ReopenNotebooks[workspace_String]/;workspaceExistQ[workspace]:=(
		reopenNotebooks0[workspace]
		)
	
reopenNotebooks0[workspace_String]:=With[
	{notebooksfile=WorkspaceNotebooksFile[workspace]},
	
	Switch[FileExistsQ[notebooksfile],
		True,reopenNotebooks[notebooksfile,workspace],
		False, Failure["NotebooksRecordFileMissing",<|"MessageTemplate"->"Notebooks file for this workspace is missing"|>],
		_, Failure["NotebooksRecordFileError",<|"MessageTemplate"->"Unable to determine notebooks file"|>]
		]
	]

reopenNotebooks[notebooksfile_,workspace_]:=
	Module[{opennotebooks,savedlist,untitledlist,notebooklist={}},
	
		opennotebooks=Get[notebooksfile];
		savedlist=DeleteDuplicates@Lookup[opennotebooks,"Saved",{}];
		untitledlist=DeleteDuplicates@Lookup[opennotebooks,"Untitled",{}];
		
		Scan[
			AppendTo[notebooklist,
				quieterNotebookOpen@#]&,
			savedlist
		];
		
		Scan[
			Block[{nb=CreateDocument[{},WindowSelected->False]},
				NotebookPut[Import[#],nb];
				SelectionMove[nb,Before,Notebook];
				AppendTo[notebooklist,nb]
				]&,
			untitledlist];
	
		notebooklist
	]
	
quieterNotebookOpen[nfn_String]:=With[{nbo=NotebookOpen[nfn,Visible->False]},
		SetOptions[nbo,Visible->True];
		nbo
	]


closeNotebooks[workspace_]:=NotebookClose/@recordableNotebooks[workspace];


End[];

EndPackage[]
