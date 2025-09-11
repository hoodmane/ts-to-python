import {
  ClassDeclaration,
  Constructor,
  ConstructSignatureDeclaration,
  Identifier,
  ImplementedKindToNodeMappings,
  InterfaceDeclaration,
  MethodSignature,
  ModuleDeclaration,
  PropertySignature,
  SyntaxKind,
  TypeAliasDeclaration,
  VariableDeclaration,
  VariableStatement,
} from "ts-morph";

export type GroupedBySyntaxKind = {
  [K in keyof ImplementedKindToNodeMappings]?: ImplementedKindToNodeMappings[K][];
};

export type InterfacesIdentifier = {
  kind: "interfaces";
  name: string;
  ifaces: InterfaceDeclaration[];
};
export type TypeAliasIdentifier = {
  kind: "typeAlias";
  name: string;
  decl: TypeAliasDeclaration;
};
export type ClassIdentifier = {
  kind: "class";
  name: string;
  decl: ClassDeclaration;
  ifaces: InterfaceDeclaration[];
};
export type VarDeclIdentifier = {
  kind: "varDecl";
  name: string;
  decl: VariableDeclaration;
  ifaces: InterfaceDeclaration[];
};

export type ClassifiedIdentifier =
  | InterfacesIdentifier
  | TypeAliasIdentifier
  | ClassIdentifier
  | VarDeclIdentifier;

export type Needed =
  | { type: "ident"; ident: Identifier }
  | { type: "interface"; ident: Identifier };

export enum Variance {
  covar = 1,
  contra = -1,
  none = 0,
}
export function reverseVariance(v: Variance): Variance {
  return -v;
}
