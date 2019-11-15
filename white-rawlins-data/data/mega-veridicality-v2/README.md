# The MegaVeridicality dataset

**Authors:** Aaron Steven White and Kyle Rawlins

**Contact:** aaron.white@rochester.edu, kgr@jhu.edu

**Version:** 2.0-alpha

**Release date:** May 23, 2018

## Overview

This dataset consists of ordinal veridicality judgments as well as ordinal acceptability judgments for 773 clause-embedding verbs of English.  The data were collected on Amazon's Mechanical Turk using [Turktools](http://turktools.net/).  

For a detailed description of the dataset, the item construction and collection methods, and discussion of how to use a dataset on this scale to address questions in linguistic theory, please see the following papers:

> White, A. S., R. Rudinger, K. Rawlins, & B. Van Durme. 2018. [Lexicosyntactic Inference in Neural Models](http://aclweb.org/anthology/D18-1501). To appear in _Proceedings of the 2018 Conference on Empirical Methods in Natural Language Processing_, Brussels, Belgium, October 31-November 4, 2018.

> White, A. S. & K. Rawlins. 2018. [The role of veridicality and factivity in clause selection](http://aaronstevenwhite.io/papers/white_role_2018.pdf). To appear in the *Proceedings of the 48th Meeting of the North East Linguistic Society*.

If you make use of this dataset in a presentation or publication, we ask that you please cite these papers.

## Version history

1.0: first public release (May 11, 2018)
2.0: alpha release (May 23, 2018)

## Manifest

- `megaveridicality_v2.csv`
- `megaveridicality_normalized_v2.csv`
- `README.md`

## Description

`megaveridicality_v2.csv` contains the raw data collected on Mechanical Turk.

| **Column**        | **Description**                                                                           | **Values**             |
|-------------------|-------------------------------------------------------------------------------------------|------------------------|
| participant       | anonymous integer identifier for participant that provided the response                   | 0...634                |
| list              | integer identifier for list participant was responding to                                 | 0...81                 |
| presentationorder | relative position of item in list                                                         | 1...68                 |
| verb              | clause-embedding verb found in the item                                                   | see paper              |
| frame             | clausal complement found in the item                                                      | see paper              |
| voice             | voice found in the item                                                                   | `active`, `passive`    |
| polarity          | polarity found in the item                                                                | `positive`, `negative` |
| conditional       | whether the item was embedded in the antecedent of a conditional (see paper)              | `True`, `False`        |
| sentence          | the sentence judged                                                                       | see paper              |
| veridicality      | ordinal scale veridicality response                                                       | `no`, `maybe`, `yes`   |
| acceptability     | ordinal scale acceptability response                                                      | 1...7                  |
| nativeenglish     | whether the participant reported speaking American English natively                       | `True`, `False`        |
| exclude           | whether the participant should be excluded based on native language                       | `True`, `False`        |

`megaveridicality_normalized_v2.csv` contains normalized veridicality and acceptability ratings, one for each verb-frame-voice-polarity tuple. These normalized ratings are constructed using an ordinal model-based normalization procedure and can be thought of as mean ratings that control for participants' differing uses of the relevant response scale. The mean log-likelihood for the 10 ratings given for each verb-frame-voice-polarity tuple under the ordinal model used to normalize the data is also given. This can be thought of as analogous to an estimate of rating variance.

| **Column**                  | **Description**                                                                           | **Values**             |
|-----------------------------|-------------------------------------------------------------------------------------------|------------------------|
| verb                        | clause-embedding verb found in the item                                                   | see paper              |
| frame                       | clausal complement found in the item                                                      | see paper              |
| voice                       | voice found in the item                                                                   | `active`, `passive`    |
| polarity                    | polarity found in the item                                                                | `positive`, `negative` |
| conditional                 | whether the item was embedded in the antecedent of a conditional (see paper)              | `True`, `False`        |
| sentence                    | the sentence judged                                                                       | see paper              |
| veridicality_normalized     | the normalized veridicality rating                                                        | [-3.52, 1.87]          |
| acceptability_normalized    | the normalized acceptability rating                                                       | [-2.85, 3.16]          |
| veridicality_loglikelihood  | the sentence judged                                                                       | [-1.74, 0.00]          |
| acceptability_loglikelihood | the sentence judged                                                                       | [-2.60, -0.06]         |

## Notes

* A javascript error produced 3 NA values for `veridicality`, none of which affect the same verb-frame pair.
