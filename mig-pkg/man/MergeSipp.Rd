\name{MergeSIPP}
\alias{MergeSIPP}
\title{Merge Sipp core data with topical modules}
\usage{
  MergeSIPP(path = "~/datasets/SIPP",
    outfile = "~/git/migration/mig-pkg/data/sipp.RData",
    heads = TRUE, test = FALSE)
}
\arguments{
  \item{path}{path/to/SIPP/folderstructure}

  \item{outfile}{where to save dataset}

  \item{heads}{TRUE if only family reference person is
  kept}

  \item{test}{TRUE if run on test dataset}
}
\description{
  takes all available SIPP panels, processes them, and
  outputs a unitary dataset The list of topical modules by
  panel and wave is
  http://www.census.gov/programs-surveys/sipp/tech-documentation/topical-modules.html
}
\details{
  Notice that the folderstructure needs to look like this:
  /Users/florianoswald/datasets/SIPP \cr |-1996\cr
  |---dat\cr |---dct\cr |---do_NBER\cr |---doc\cr
  |---dta\cr |-----core_and_topical\cr |-----export\cr
  |-2001\cr |---dat\cr |---dct\cr |---do_NBER\cr |---doc\cr
  |---dta\cr |-----core_and_topical\cr |-----export\cr
  |-2004\cr |---dat\cr |---dct\cr |---do_NBER\cr |---doc\cr
  |---dta\cr |-----core_and_topical\cr |-----export\cr
  |-2008\cr |---dat\cr |---dct\cr |---do_NBER\cr |---doc\cr
  |---dta\cr |-----core_and_topical\cr |-----export\cr in
  particular, the paths yyyy/dta/export must exist
}

