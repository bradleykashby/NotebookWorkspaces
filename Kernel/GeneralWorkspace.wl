(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`GeneralWorkspace`"]


(*$GeneralWorkspace
LoadGeneralWorkspace
PruneGeneralNotebookUUIDs
AddNotebookToGeneralList
$GeneralNotebookUUIDs
*)
Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]
Needs["BradleyAshby`NotebookWorkspaces`Common`"]


$GeneralWorkspace= "GeneralWorkspace";
$localgeneraluuids="NotebookWorkspaces/$GeneralNotebookUUIDs";

$GeneralNotebookUUIDs/:Set[$GeneralNotebookUUIDs,value_]:=(
	LocalSymbol[$localgeneraluuids]=value;
	UpdatePalette[$GeneralNotebookUUIDs];
	value)
$GeneralNotebookUUIDs:=LocalSymbol[$localgeneraluuids]/.{x_List:>x,_->{}}

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


generalActiveQ[]:=TrueQ[WorkspaceMetadata[$GeneralWorkspace,"FEPID"]==$FEPID]


LoadGeneralWorkspace[args___]/;!workspaceExistQ[$GeneralWorkspace]:=
	CreateWorkspace[$GeneralWorkspace]

LoadGeneralWorkspace[log_String:"Loading General Workspace"]:=
	With[{notebooklist=ReopenNotebooks[$GeneralWorkspace]},
		RecordWorkspaceMetadata[log];
		AddNotebookToGeneralList[notebooklist]
	]


PruneGeneralNotebookUUIDs[]:=
	With[{currentgeneral=Information[GeneralNotebooks[],"ExpressionUUID"]},
		$GeneralNotebookUUIDs=currentgeneral
	]


SetAttributes[AddNotebookToGeneralList,Listable];
AddNotebookToGeneralList[nb_NotebookObject]:=
	AddNotebookToGeneralList[Information[nb,"ExpressionUUID"]]

AddNotebookToGeneralList[nbuuid_String]:=
	Module[{newlist},
		newlist=DeleteDuplicates[Append[$GeneralNotebookUUIDs,nbuuid]];
		$GeneralNotebookUUIDs=newlist;
		
		GeneralNotebooks[]
	]


RemoveNotebookFromGeneral[nb_NotebookObject]:=
	RemoveNotebookFromGeneral[Information[nb,"ExpressionUUID"]]

RemoveNotebookFromGeneral[nbuuid_String]:=
	With[{nbo=SelectFirst[Notebooks[],Information[#,"ExpressionUUID"]==nbuuid&]},
		removeNotebookFromGeneralRecord[nbo];
		$GeneralNotebookUUIDs=DeleteCases[$GeneralNotebookUUIDs,nbuuid];
	
		GeneralNotebooks[]
	]


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
