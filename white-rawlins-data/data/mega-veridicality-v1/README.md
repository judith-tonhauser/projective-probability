# The MegaAttitude dataset

**Authors:** Aaron Steven White and Kyle Rawlins

**Contact:** aaron.white@rochester.edu, kgr@jhu.edu

**Version:** 1.0

**Release date:** May 11, 2018

## Overview

This dataset consists of ordinal veridicality judgments as well as ordinal acceptability judgments for 517 clause-embedding verbs of English.  The data were collected on Amazon's Mechanical Turk using [Turktools](http://turktools.net/).  

For a detailed description of the dataset, the item construction and collection methods, and discussion of how to use a dataset on this scale to address questions in linguistic theory, please see the following paper:

White, A. S. & K. Rawlins. 2018. [The role of veridicality and factivity in clause selection](http://aaronstevenwhite.io/papers/white_role_2018.pdf). To appear in the *Proceedings of the 48th Meeting of the North East Linguistic Society*.

If you make use of this dataset in a presentation or publication, we ask that you please cite this paper.

## Version history

1.0: first public release, May 11, 2018.

## Description

| **Column**        | **Description**                                                                           | **Values**          |
|-------------------|-------------------------------------------------------------------------------------------|---------------------|
| participant       | anonymous integer identifier for participant that provided the response                   | 0...290             |
| list              | integer identifier for list participant was responding to                                 | 0...15             |
| presentationorder | relative position of item in list                                                         | 1...68              |
| verb              | clause-embedding verb found in the item                                                   | see paper           |
| frame             | clausal complement found in the item                                                      | see paper           |
| voice             | voice found in the item                                                                   | `active`, `passive` |
| polarity            | polarity found in the item                                                                   | `positive`, `negative` |
| conditional            | whether the item was embedded in the antecedent of a conditional (see paper)                                                                   | `True`, `False` |
| veridicality          | ordinal scale veridicality response                                                      | `no`, `maybe`, `yes`               |
| acceptability          | ordinal scale acceptability response                                                      | 1...7               |
| nativeenglish     | whether the participant reported speaking American English natively                       | `True`, `False`     |
| exclude           | whether the participant should be excluded based on native language | `True`, `False`     |
