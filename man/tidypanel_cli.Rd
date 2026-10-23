\name{tidypanel_cli}
\alias{tidypanel_cli}
\title{Run the Local JSON Cleaning Interface}
\usage{
tidypanel_cli(args = commandArgs(trailingOnly = TRUE), protected_roots = character())
}
\arguments{
\item{args}{Character arguments: \code{describe}, or \code{clean --request PATH}. Use \code{-} for standard input. \code{clean} alone also reads standard input.}
\item{protected_roots}{Additional existing directories where output is forbidden. The installed package directory is always protected. The launcher adds its own package directory.}
}
\value{Invisibly, an integer exit status: 0 for success, 2 for invalid requests, 3 for an edition mismatch, 4 for cleaning failures, or 5 for output failures. The function does not terminate R.}
\description{A local adapter for the public table cleaner. It reads one bounded UTF-8 JSON request and writes one JSON response. It does not evaluate request text, start a service or upload data.}
\details{
Run \code{describe} for the JSON request schema. Select one local file and a new output directory with an existing parent. Explicitly supply \code{na_strings} from the source declaration. Only public reader options are exposed. Preview values are opt-in and may contain sensitive data.

The adapter writes full cleaned R objects, a convenience CSV, a tabular audit and column metadata. CSV does not preserve every R type or missing-value distinction. The RDS file is for compatible trusted R use. A failed write can leave a partial new output directory; it is not deleted or reused. The audit is not a complete cell-by-cell history. This interface inherits the caller's filesystem permissions and is not a sandbox.

Use the bundled \code{exec/tidypanel.R} launcher with \code{Rscript --vanilla}. It requires the matching installed public source edition and exits with the returned status. Cleaner output other than the JSON response is directed to standard error. Warnings are recorded in the response, with bounded messages.
}
\examples{
\dontrun{
tidypanel_cli("describe")
}
}
