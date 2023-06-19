(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`GeneralWorkspace`"]


$GeneralWorkspace
initializeGeneralWorkspace
PruneGeneralNotebookUUIDs
$GeneralNotebookUUIDs
AddNotebookToGeneralList

Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]
Needs["BradleyAshby`NotebookWorkspaces`Configuration`"]
Needs["BradleyAshby`NotebookWorkspaces`WorkspaceManagement`"]
Needs["BradleyAshby`NotebookWorkspaces`SaveAndRecordNotebooks`"]
Needs["BradleyAshby`NotebookWorkspaces`Palette`"]


$GeneralWorkspace= "GeneralWorkspace";
$localgeneraluuids="NotebookWorkspaces/$GeneralNotebookUUIDs";

$GeneralNotebookUUIDs/:Set[$GeneralNotebookUUIDs,value_]:=(
	LocalSymbol[$localgeneraluuids]=value;
	UpdateDynamicsUsing[$GeneralNotebookUUIDs];
	value)
$GeneralNotebookUUIDs:=LocalSymbol[$localgeneraluuids]

(*backwards compatibility:
	moving any stored $GeneralNotebookUUIDs to the new persistence method*)
With[{initval=InitializationValue[$GeneralNotebookUUIDs,"FrontEndSession"]},
	If[!MissingQ[initval],
		$GeneralNotebookUUIDs=initval;
		Remove[InitializationValue[$GeneralNotebookUUIDs,"FrontEndSession"]]
	]
]

$GeneralNotebooks:=Select[Notebooks[],MemberQ[$GeneralNotebookUUIDs,Information[#,"ExpressionUUID"]]&]


GeneralNotebooks[]:=$GeneralNotebooks


initializeGeneralWorkspace[]/;!ListQ[$GeneralNotebookUUIDs]:=(
	$GeneralNotebookUUIDs={};
	If[workspaceExistQ[$GeneralWorkspace],
		ReopenNotebooks[$GeneralWorkspace],
		CreateWorkspace[$GeneralWorkspace]
	]
	)


PruneGeneralNotebookUUIDs[]:=
	With[{currentgeneral=Information[GeneralNotebooks[],"ExpressionUUID"]},
		$GeneralNotebookUUIDs=currentgeneral
	]


SetAttributes[AddNotebookToGeneral,Listable];
AddNotebookToGeneral[nb_NotebookObject]:=
	AddNotebookToWorkspace[$GeneralWorkspace,nb]


SetAttributes[AddNotebookToGeneral,Listable];
AddNotebookToGeneralList[nb_NotebookObject]:=
	AddNotebookToGeneralList[Information[nb,"ExpressionUUID"]]

AddNotebookToGeneralList[nbuuid_String]:=
	Module[{newlist},
		initializeGeneralWorkspace[];
		newlist=DeleteDuplicates[Append[$GeneralNotebookUUIDs,nbuuid]];
		$GeneralNotebookUUIDs=newlist;
		
		GeneralNotebooks[]
	]


RemoveNotebookFromGeneral[nb_NotebookObject]:=RemoveNotebookFromGeneral[Information[nb,"ExpressionUUID"]]

RemoveNotebookFromGeneral[nbuuid_String]:=(
	initializeGeneralWorkspace[];
	$GeneralNotebookUUIDs=DeleteCases[$GeneralNotebookUUIDs,nbuuid];

	GeneralNotebooks[]
)


(*inverse of RecordNotebookToWorkspace, currently only used in GeneralWorkspace*)
removeNotebookFromGeneralRecord[nb_NotebookObject]:=
	With[{workspace=$GeneralWorkspace},
		Module[{key,filepath=notebookFile[nb,workspace],oldrecord,newrecord,newlist},
			
			key=Switch[FileExistsQ[Quiet[NotebookFileName[nb]]],
				True,"Saved",
				False,"Untitled"];
			
			oldrecord=Get@WorkspaceNotebooksFile[workspace];
			newlist=DeleteCases[Lookup[oldrecord,key,{}],filepath];
			newrecord=Append[oldrecord,key->newlist];
			
			Put[newrecord,WorkspaceNotebooksFile[workspace]];
			
			newrecord
		]
	]


End[];

EndPackage[]
