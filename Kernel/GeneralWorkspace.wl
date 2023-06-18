(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`GeneralWorkspace`"]


$GeneralWorkspace
initializeGeneralWorkspace
PruneGeneralNotebookUUIDs
$GeneralNotebookUUIDs

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
AddNotebookToGeneral[nb_NotebookObject]:=AddNotebookToGeneral[Information[nb,"ExpressionUUID"]]

AddNotebookToGeneral[nbuuid_String]:=
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


End[];

EndPackage[]
