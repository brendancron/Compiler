
Import(module) exists in the metaprocessor. However, it DOES NOT exist in the expanded AST, so the metaprocessor is responsible for processing any included files. Unlike rust, files are only included when they are explicitly exported.

## Graph Construction
Module Table: Map path -> Module
When processing a file:
* Parse -> AST
* Extract import nodes?
* For each import:
	* Resolve to cannonical path
	* Add edge current -> imported
	* If unseen: recursively process imported files
* Result a directed graph where nodes = file, edges = imports.
