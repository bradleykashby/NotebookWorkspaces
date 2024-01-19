(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`Utilities`"]


ReinitializePalette
UpdatePalette
WorkspaceExistsQ


Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]


SetAttributes[FindDynamicsUsing,{Listable,HoldAll}]
FindDynamicsUsing[sym_Symbol]:=Cases[Internal`GetTrackedIDs[],{id_Integer,{___, HoldPattern[sym],___}}:>id]

SetAttributes[UpdateDynamicsUsing,{Listable,HoldAll}]
UpdateDynamicsUsing[sym_Symbol]:=FrontEndExecute@FrontEnd`UpdateDynamicObjects[Flatten[FindDynamicsUsing[sym]]]

UpdateDynamicsUsing[box_BoxObject]:=(
	FrontEndExecute@FrontEnd`UpdateDynamicObjects[{box}];
	)


SetAttributes[UpdatePalette,{Listable,HoldAll}]

UpdatePalette[sym_]:=(
	If[FindDynamicsUsing[sym]=={},
		ReinitializePalette[]];
		
	UpdateDynamicsUsing[sym];
	)

ReinitializePalette[]:=With[{
	palette=Select[Notebooks[],
			AbsoluteCurrentValue[#,{TaggingRules,"NotebookWorkspacesPaletteQ"}]&]},
		
		If[palette!={},
			NotebookPut[NotebookGet[#],#]&/@palette]
	]


WorkspaceExistsQ[_Symbol]:=False;
WorkspaceExistsQ[workspace_String]:=FileExistsQ[WorkspaceNotebooksFile[workspace]];


End[]

EndPackage[]
