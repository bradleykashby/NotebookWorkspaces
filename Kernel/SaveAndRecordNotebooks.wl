(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`SaveAndRecordNotebooks`"]

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


	(* Untitled/unsaved notebooks *)
SaveNotebook[nb_NotebookObject,workspace_String]/;FailureQ[Quiet[NotebookFileName[nb]]]:=
	Module[{
		nbtitle=Quiet[Information[nb,"WindowTitle"]],
		filepath
		},
		
		If[!MissingQ@nbtitle,
			filepath=FileNameJoin[{
				WorkspaceSaveDirectory[workspace],
				ResourceFunction["SlugifyString"][nbtitle]<>".nb"}];
			Export[filepath,nb,OverwriteTarget->True]
			]
	]; 

	(* Notebooks with an existing save location *)
SaveNotebook[nb_NotebookObject,workspace_String]/;!FailureQ[Quiet[NotebookFileName[nb]]]:=(
	NotebookSave[nb];
	)


(* Make a list of open notebooks to recover in case of crash *)
	(* Untitled/unsaved notebooks *)
RecordNotebook[nb_NotebookObject,workspace_String]/;FailureQ[Quiet[NotebookFileName[nb]]]:=
	Module[{nbtitle=Quiet[Information[nb,"WindowTitle"]],filepath},
		
		If[!MissingQ@nbtitle,
			filepath=FileNameJoin[{
				WorkspaceSaveDirectory[workspace],
				ResourceFunction["SlugifyString"][nbtitle]<>".nb"}];
			AppendTo[opennotebooks["Untitled"],filepath]
			]		
	];

	(* Notebooks with an existing save location *)
RecordNotebook[nb_NotebookObject,workspace_String]/;!FailureQ[Quiet[NotebookFileName[nb]]]:=AppendTo[opennotebooks["Saved"],NotebookFileName[nb]];


systemNotebookQ[nb_NotebookObject]:=TrueQ@With[{dir=Quiet@NotebookDirectory@nb},
	And[
		StringQ@dir,
		!StringFreeQ[dir,
			Alternatives[
				$BaseDirectory,
				$InstallationDirectory,
				$UserBasePacletsDirectory
			]
		]
	]
];


recordableNotebooks[workspace_String]/;(workspace==$GeneralWorkspace):=$GeneralNotebooks;

recordableNotebooks[HoldPattern[workspace_:$CurrentWorkspace]]:=With[
	{general=$GeneralNotebookUUIDs,
	excluded=$ExcludedNotebooks},

		Select[Notebooks[],
			Module[{title,type,uuid},
				{title,type,uuid}=Information[#,
					{"WindowTitle","DocumentType","ExpressionUUID"}];	
				
				And[
					Not[MemberQ[excluded,title]],
					Not[MemberQ[general,uuid]],
					Not[type=="Help"],
					TrueQ[CurrentValue[#,Visible]],
					Not[systemNotebookQ[#]]
			        ]
				]&]];


SaveAndRecordNotebooks::duplicates="Duplicate notebooks saved. Not all will be recovered.";
SaveAndRecordNotebooks::opennotebooks="Too few open notebooks. Notebooks saved but not recorded.";

SaveAndRecordNotebooks[allq_Symbol:False]/;BooleanQ[allq]:=SaveAndRecordNotebooks[allq,$CurrentWorkspace]
SaveAndRecordNotebooks[allq_Symbol:False,_Symbol]:=Failure["NoWorkspace",<|"MessageTemplate"->"No workspace loaded"|>]

SaveAndRecordNotebooks[allq_Symbol:False,workspace_String]:=With[
			{workspacelastsaved=Lookup[WorkspaceMetadata[workspace,"SaveInformation"],"LastSaved"]},
		
		saveAndRecordNotebooks[allq,#,workspacelastsaved]&/@DeleteDuplicates[{$GeneralWorkspace,workspace}]
		];

saveAndRecordNotebooks[allq_Symbol:False,workspace_String,workspacelastsaved_]:=Module[{
			recordablenotebooks,
			saveablenotebooks},

		recordablenotebooks=recordableNotebooks[workspace];
		If[Or[allq,MissingQ[workspacelastsaved]],
			saveablenotebooks=recordablenotebooks,
			
			saveablenotebooks=Select[recordablenotebooks,
				Module[{modifiedq,modtime},
				{modifiedq,modtime}=Information[#,
					{"ModifiedInMemory","MemoryModificationTime"}];	
				
				And[
					modifiedq,
					workspacelastsaved<modtime
			        ]
				]&]
			];
		
		opennotebooks=<|"Untitled"->{},"Saved"->{},"Timestamp"->Now,"Workspace"->workspace,"FEPID"->$FEPID|>;

		SaveNotebook[#,workspace]&/@saveablenotebooks;
		RecordNotebook[#,workspace]&/@recordablenotebooks;

		If[!DuplicateFreeQ[Flatten@Lookup[opennotebooks,{"Untitled","Saved"}]],
			Message[SaveAndRecordNotebooks::duplicates]
			];
		
		Put[opennotebooks,WorkspaceNotebooksFile[workspace]];
		
		KeyDrop[opennotebooks,"FEPID"]

]


ReopenNotebooks[workspace_String]/;workspaceExistQ[workspace]:=(
		reopenNotebooks0[workspace]
		);
	
reopenNotebooks0[workspace_String]:=With[
	{notebooksfile=WorkspaceNotebooksFile[workspace]},
	
	Switch[FileExistsQ[notebooksfile],
		True,reopenNotebooks[notebooksfile,workspace],
		False, Failure["NotebooksRecordFileMissing",<|"MessageTemplate"->"Notebooks file for this workspace is missing"|>],
		_, Failure["NotebooksRecordFileError",<|"MessageTemplate"->"Unable to determine notebooks file"|>]
		]
	];

reopenNotebooks[notebooksfile_,workspace_]:=
	Module[{opennotebooks,savedlist,untitledlist,notebooklist={}},
	
		opennotebooks=Get[notebooksfile];
		savedlist=opennotebooks["Saved"];
		untitledlist=opennotebooks["Untitled"];
		
		Scan[
			AppendTo[notebooklist,
				quieterNotebookOpen@#]&,
			savedlist
		];
		
		Scan[
			Block[{nb=CreateDocument[{},WindowSelected->False]},
				NotebookPut[Import[#],nb];
				AppendTo[notebooklist,nb]
				]&,
			untitledlist];
	
		If[workspace==$GeneralWorkspace,
			AddNotebookToGeneral[notebooklist];
		]
	];
	
quieterNotebookOpen[nfn_String]:=With[{nbo=NotebookOpen[nfn,Visible->False]},
		SetOptions[nbo,Visible->True];
		nbo
	]


closeNotebooks[workspace_]:=NotebookClose/@recordableNotebooks[workspace];


End[];

EndPackage[]
