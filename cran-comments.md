# Release (v0.2.118)

Mixed compact codes with zero-padded evidence retain their original text.
Unsupported name-first reference exports require an identifier column first.
Subtotal-label checks now avoid repeated per-record matching in large tables.
Established row-selection rules are unchanged.

Unsupported slash-dated series exports now require a separately prepared named
observation table instead of merging metadata into column labels.

This release includes reviewed baseline cleaning fixes and three independently
invented demonstrations, each with three fictional input records. Examples
cover numbers, dates, and subtotal rows with optional wide-to-long reshaping.
Additional aggregate-table regressions were added to the separate validation
suite. No development corpus or validation material is distributed.

Baseline fixes preserve clock precision, complete date lexemes, explicit
identifier suffixes, literal quality flags, and unit-qualified precipitation
totals. Fractional serial days are not silently reduced to dates.

This release ships exactly three small fictional CSV demonstrations. Training
data, validation datasets, regression fixtures, and development test sources
are not included. Regression validation is performed separately before release.

The examples demonstrate numeric cleaning, calendar dates, and subtotal removal
with optional wide-to-long reshaping. No personal records are distributed.

Cleaning fixes retain measurement labels, paired free/total measurements, and
complete headerless categorical records. Numeric report sections must be
provided as plain tables rather than silently mixed with report metadata.
Commented delimited headers preserve labels and quoted field boundaries without
turning comment-prefix whitespace into extra columns.
Explicit identifier suffixes are protected from approximate financial mappings.
Sampled averages are retained when a time axis and integer observation-count
fields establish a measurement context. Delimited metadata preambles require
a plain observation table rather than being silently interpreted as records.
Headerless whitespace records with quoted labels require CSV or an explicitly
named data frame. Mixed separators are rejected instead of silently collapsing
numeric fields and losing records.
Numeric-context `NaN` missing markers are recognized while pure text labels are
retained. Scientific unit descriptor rows require a separately prepared named
observation table with an appropriate missing-value lexicon for literal codes.
Hyphenated English month dates are checked as complete calendar values without
depending on the time locale. Explicit date identifiers remain text. Qualified
population income measures are not collapsed into corporate-profit labels.
Sparse country-year records are retained, including missing-only measurements.
Explicitly qualified total-energy measurements are retained rather than removed
as subtotals. Missing calendar fields are not filled from adjacent observations.
Lowercase numeric missing markers no longer force otherwise numeric year columns
or pivoted values to text. Pure categories and recognized identifiers retain
literal lowercase markers; the user-supplied missing lexicon remains configurable.

Author and contact metadata use Tony Lu and xulunt123@gmail.com. Build metadata
uses a neutral public identity, without a local account name or path.
