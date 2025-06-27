import Format
open PrettyFormat

-- run with:
-- lake exe ProjectFormat -folder . -include .lake/build/lib  -filesPrWorker 1 -workers 16
--
namespace Batteries
#coreFmt Batteries.Tactic.Alias.alias fun
| #[modifiers, aliasAtom, aliasIdent, assignAtom, nameIdent] =>
  return ((modifiers) ?> ""<**>"")<> aliasAtom <> PrettyFormat.nestDoc 2 (" "
  <> aliasIdent <> " "
  <> assignAtom <> (" " <^> PrettyFormat.Doc.nl) <> nameIdent )
| _ =>
  failure

#coreFmt Batteries.Tactic.Alias.aliasLR fun
| #[modifiers, aliasAtom, lpar, binder, comma, right ,rpar,assignAtom, nameIdent] => do
  let binder ← PrettyFormat.formatStx binder
  return (modifiers ?> (" " <^> PrettyFormat.Doc.nl)) <> aliasAtom <> PrettyFormat.nestDoc 2 (" "
  <> lpar <> binder <> comma <> " " <> right <> rpar <> " "
  <> assignAtom <**> nameIdent)
| _ =>
  failure
#coreFmt Batteries.Tactic.seq_focus fun
| #[splitAtom, separatorAtom, lparen, proof, rparen] =>

  return splitAtom <_> separatorAtom <**> lparen <> (addSpaceAfterDelimiter (fun s => s == ";") proof.getArgs) <> rparen
| _ => failure
#coreFmt Batteries.Tactic.Lint.nolint combine' (· <_> ·)

#coreFmt Batteries.CodeAction.tactic_code_action fun
| #[nameAtom, args] =>
  return nameAtom <_> combine (.<_>.) args.getArgs
| _ => failure
end Batteries

unsafe def main (originalArgs : List String) : IO (Unit) :=
  formatMain originalArgs
