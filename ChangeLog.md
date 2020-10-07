# Changelog for dynmod

### 0.3.2.0
* Parse NodeColors of the form #123456. 

### 0.3.1.0
* Let Node coordinates be negative. 

### 0.3.0.0
* Completed initial graphics workflow: The -g,--gml option writes out the
  structure, coordinates, and colors of the parsed dmms file as a gml file. This
  can be opened in yED, a network layout & formating tool. The -u,--update
  SOURCE_FILE option then reads in that altered gml file and updates the colors
  and coordinates of the parsed dmms file, so long as they are the same network. 

### 0.2.6.2
* Bug fixes, properly parse GateLits

### 0.2.6.0
* Preliminary symbol legends in the supplementary materials pdf. Removed the
  Typeable code and replaced it with [minBound ..]

### 0.2.5.1
* Added the following NodeTypes with Codes:
* GEF	GEF
* GAP	GAP
* GTPase	GTPa
* Enzyme	Enz
* Added the following LinkTypes with Codes:
* GEF_Activity	GEF
* GAP_Activity	GAP
* Ligand_Binding	Ligand
* Proteolysis	Lysis
* Catalysis	Cat
* Binding_Localizaton	BLoc

### 0.2.5.0
* Add to the standard output a .booleannet file for those ModelLayers of a
  DMModel whose every DMNode has a boolean NodeGate.

### 0.2.0.0
* Added the ability, with the -s|-supplementary option, to generate & write to
  disk a .tex & .bib file, which typeset to a supplementary materials pdf, with
  references.
