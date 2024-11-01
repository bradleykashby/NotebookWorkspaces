(* ::Package:: *)

BeginPackage["BradleyAshby`NotebookWorkspaces`Palette`"]


palettecontents

Begin["`Private`"]


Needs["BradleyAshby`NotebookWorkspaces`"]
Needs["BradleyAshby`NotebookWorkspaces`Common`"]


workspaceslist:=Keys@ReverseSortBy[KeyDrop[$GeneralWorkspace]@WorkspaceMetadata[],#["Timestamp"]&];


sortedgeneral:=SortBy[GeneralNotebooks[],
	{MissingQ[Information[#,"FileName"]]&,Information[#,"FileName"]&}]


generalpalettebuttons:=
	{
		Button[Information[#,"WindowTitle"],SetSelectedNotebook[#]],
		Tooltip[Button[Style["x",Red],RemoveNotebookFromGeneral[#]],"Remove from general space"]
	}&/@sortedgeneral


excludedbuttons:=
	{Tooltip[Button[Style["x",Red],RemoveFromExcludedNotebooks[#],Appearance->"Palette"],"Remove from excluded list"],
		#		
	}&/@$ExcludedNotebooks


nameWorkspaceDialog[workspace_String:""]:=DialogInput[{workspacename=workspace,error=Spacer[1]},
		Column[{
			TextCell["Enter a name for the new workspace: "],
			InputField[Dynamic[workspacename],String,BoxID->"namefield"],
			Item[Dynamic[error],ItemSize->Automatic],
			Row[{
				DefaultButton[error=Enclose[
					Confirm[validateWorkspaceName[workspacename]];
					DialogReturn[workspacename],Function[failure,failure["Expression"]["StyledMessage"]]]],
				CancelButton[]}]
		},ItemSize->Full,BaseStyle->{FontSize->12}],
	WindowTitle->"Name the worskspace",WindowSize->{250,All},
	Initialization:>(MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[FE`BoxReference[EvaluationNotebook[],{{"namefield"}},FE`BoxOffset->{FE`BoxChild[1]},FE`SearchStart->"StartFromBeginning"]]])]


saveWorkspaceAsButtonFunc[]:=
		With[{newname=nameWorkspaceDialog[]},SaveWorkspaceAs[newname]];


currentSpaceInfo[workspace_String]/;StringQ[$WorkspaceStatus]:=Splice[{
				Style[workspace,"Author"],
				$WorkspaceStatus
			}]

currentSpaceInfo[workspace_String]:=Splice[{
				Style[workspace,"Author"],
				Row[{
					Button["Save notebooks",SaveWorkspace[True]],
					Button["Close workspace",CloseWorkspace[]]}],
				Row[{
					"Last saved: ",
					workspaceLastSaved[workspace,"String"]}]
			}]

currentSpaceInfo[workspace_]:=Style["No workspace loaded","Author"]


workspacesTab[workspacemetadata_Association]:=
	Column[{
		currentSpaceInfo[$CurrentWorkspace],
		Spacer[1],
		Style["Load a workspace:","Author"],
		Column[Button[#,LoadWorkspace[#],Method->"Queued"]&/@DeleteCases[workspaceslist,$CurrentWorkspace]],
		Spacer[1],
		Style["Save to new workspace","Author"],
		Button["Save current state as...",saveWorkspaceAsButtonFunc[],Method->"Queued"]
	}]

workspacesTab[_]:=Column[{Style["Workpace metadata not loaded","Author"]}]


generalTab:=Column[{
	Style["Manage General Workspace","Author"],
	Grid@generalpalettebuttons,
	Spacer[1],
	Button["Add selected notebook", AddNotebookToGeneral[SelectedNotebook[]]]
}]


configureTab:=Column[{
	Style["Workspace Configuration","Author"],
	"Base save directory:",
	Row[{Spacer[10],Button[$BaseSaveDirectory,SystemOpen[$BaseSaveDirectory],Appearance->"Frameless"]}],
	"Default workspace name:",
	Row[{Spacer[10],$DefaultWorkspace}],
	"Save frequency:",
	Row[{Spacer[10],$SaveFrequency}],
	Spacer[1],
	Button["Change configuration",ConfigureWorkspaces[],Method->"Queued"],
	Spacer[1],
	Style["Excluded Notebooks:","Author"],
	
	excludedbuttons/.{{}->"No excluded notebooks",buttons_:>Grid[Prepend[buttons,{"","Notebook Title:"}],Alignment->Left]},
	
	Spacer[1],
	Button["Add selected notebook", AddToExcludedNotebooks[SelectedNotebook[]]]
	}]


palettecontents=Panel[TabView[{
		"Workspaces"->Dynamic[workspacesTab[WorkspaceMetadata[]],
			TrackedSymbols:>{
				$WorkspaceMetadata,
				$CurrentWorkspace,
				$WorkspaceStatus}],
		"General space"->Dynamic[generalTab,
			TrackedSymbols:>{
				$GeneralNotebookUUIDs}],
		"Configuration"->Dynamic[configureTab,
			TrackedSymbols:>{
				$DefaultWorkspace,
				$SaveFrequency,
				$BaseSaveDirectory,
				$ExcludedNotebooks}]
		}],BaseStyle->{FontSize->12}]


createPaletteNB[]:=
	CreatePalette[
		PaletteNotebook[
			Dynamic[BradleyAshby`NotebookWorkspaces`Palette`palettecontents],
			Initialization:>
				(Needs["BradleyAshby`NotebookWorkspaces`"->None]),
			NotebookDynamicExpression:>Refresh[
				With[{aop=AbsoluteCurrentValue[$FrontEnd,"AutoOpenPalettes"]},
					CurrentValue[$FrontEnd,"AutoOpenPalettes"]=DeleteCases[aop,"NotebookWorkspacesPalette.nb"]];,
				None],
			TaggingRules->{"NotebookWorkspacesPaletteQ"->True}],
		WindowSize->Fit]


End[]

EndPackage[]
