(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`GeneralWorkspace`"]


$GeneralWorkspace
$GeneralNotebooks
$GeneralNotebookUUIDs
initializeGeneralWorkspace

Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]
Needs["BradleyAshby`NotebookWorkspaces`Configuration`"]
Needs["BradleyAshby`NotebookWorkspaces`WorkspaceManagement`"]
Needs["BradleyAshby`NotebookWorkspaces`SaveAndRecordNotebooks`"]
Needs["BradleyAshby`NotebookWorkspaces`Palette`"]


$GeneralWorkspace= "GeneralWorkspace";

$GeneralNotebookUUIDs;
$GeneralNotebooks:=Select[Notebooks[],MemberQ[$GeneralNotebookUUIDs,Information[#,"ExpressionUUID"]]&]


GeneralNotebooks[]:=$GeneralNotebooks


initializeGeneralWorkspace[]/;!ListQ[$GeneralNotebookUUIDs]:=(
	InitializationValue[$GeneralNotebookUUIDs,"FrontEndSession"]=$GeneralNotebookUUIDs={};
	UpdateDynamicsUsing[$GeneralNotebooks];
	If[workspaceExistQ[$GeneralWorkspace],
		ReopenNotebooks[$GeneralWorkspace],
		CreateWorkspace[$GeneralWorkspace]
	]
);


SetAttributes[AddNotebookToGeneral,Listable];
AddNotebookToGeneral[nb_NotebookObject]:=AddNotebookToGeneral[Information[nb,"ExpressionUUID"]]
AddNotebookToGeneral[nbuuid_String]:=(
	initializeGeneralWorkspace[];
	InitializationValue[$GeneralNotebookUUIDs,"FrontEndSession"]=$GeneralNotebookUUIDs=DeleteDuplicates[Append[$GeneralNotebookUUIDs,nbuuid]];
	UpdateDynamicsUsing[$GeneralNotebooks];
	GeneralNotebooks[]
)

RemoveNotebookFromGeneral[nb_NotebookObject]:=RemoveNotebookFromGeneral[Information[nb,"ExpressionUUID"]]
RemoveNotebookFromGeneral[nbuuid_String]:=(
	initializeGeneralWorkspace[];
	InitializationValue[$GeneralNotebookUUIDs,"FrontEndSession"]=$GeneralNotebookUUIDs=DeleteCases[$GeneralNotebookUUIDs,nbuuid];
	UpdateDynamicsUsing[$GeneralNotebooks];
	GeneralNotebooks[]
)

generalnotebooknames:=Information[#,"WindowTitle"]&/@$GeneralNotebooks


End[];

EndPackage[]
