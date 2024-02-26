# A script to create a list of publications collected in the `_publications.yml`. 
library(magrittr)

# function to set up a link acting as a button, given a publication item
button <- function(item) {
  # there might be three type of links: url, doi, or arXiv
  links <- c(
    url = item$url,
    doi = item$doi,
    abstract = item$abstract,
    pdf = item$pdf,
    arxiv = item$arxiv,
    github = item$github
  )

  purrr::map2_chr(links, names(links), ~ {
    href <- .x
    name <- .y
    if (name == "pdf") {
      icon_name <- stringr::str_glue('<i class="fas fa-file-pdf"></i> {name}')
    }
    else {
      if (name == "url") {
        icon_name <- stringr::str_glue('<i class="fa fa-link" aria-hidden="true"></i> {name}')
      }
      else {
        if (name == "arxiv") {
          icon_name <- stringr::str_glue('<i class="ai ai-arxiv"></i> {name}')
        }
        else {
          if (name == "github") {
            icon_name <- stringr::str_glue('<i class="fa-brands fa-github"></i> {name}')
          }
          else {
            icon_name <- name
          }
        }
      }
    }
    stringr::str_glue('<a  target="_blank" href="{href}"><button class="ref-btn {name}-btn">{icon_name}</button></a>')
  }) %>%
    stringr::str_c(collapse = " ")
}

# function to format a publication item
format_publication <- function(item, type_) {
  if (!type_ == "presentation") {
    # formatting list of authors
    authors <- item$author %>%
      purrr::map(~ ifelse(.x == "me", "**Ryeznik, Y.**", .x)) %>%
      stringr::str_c(collapse = ", ")
  }
  else {
    authors <- item$author[!item$author == "me"] %>%
      stringr::str_split(", ") %>%
      purrr::map(rev) %>%
      purrr::map(stringr::str_c, collapse = " ") %>%
      stringr::str_c(collapse = " and ")
  }

  # formatting year
  year <- stringr::str_glue('({item$year})')

  # formatting title
  title <- stringr::str_glue('"{item$title}"')

  # formatting journal name
  journal <- item$journal
  if (!is.null(journal)) {
    journal <- stringr::str_glue('_{item$journal}_')
  }
  else{
    journal <- ""
  }
  
  # formatting ISSN
  issn <- item$issn
  if (!is.null(issn)) {
    issn <- stringr::str_glue('ISSN {issn}')
  }

  # formatting volume + number
  volume <- item$volume
  number <- item$number
  if (!is.null(volume)) {
    if (!is.null(number)) {
      volume_number <- stringr::str_glue('**{volume} ({number})**')
    }
    else {
      volume_number <- stringr::str_glue('**{volume}**')
    }
  }
  else {
    volume_number <- ""
  }

  # getting pages
  pages <- item$pages
  if (!is.null(pages)) {
    volume_number_pages <- stringr::str_c(volume_number, pages, sep = ":")
  }
  else {
    volume_number_pages <- volume_number
  }

  # getting language
  language <- item$language
  if (!is.null(language)) {
    language <- stringr::str_glue('(in {language})')
  }

  # for conferences:
  # getting venue
  venue <- stringr::str_glue('_{item$venue}_')

  # getting country
  country <- item$country

  # getting city
  city <- item$city

  # getting date
  date_ <- item$date

  # for books, book chapters, and tutorials
  # getting publisher
  publisher <- item$publisher

  # getting book title
  book <- item$book

  # getting chapter
  chapter <- item$chapter

  if (type_ == "thesis") {
    out <- authors %>%
      stringr::str_c(year, sep = " ") %>%
      stringr::str_c(title, sep = " ") %>%
      stringr::str_c(journal, sep = " ") %>%
      stringr::str_c(issn, sep = ",") %>%
      stringr::str_c(volume_number_pages, sep = " ") %>%
      stringr::str_c(button(item), sep = ". ")

  }
  else {
    if (type_ == "paper") {
      out <- authors %>%
        stringr::str_c(year, sep = " ") %>%
        stringr::str_c(title, sep = " ") %>%
        stringr::str_c(journal, sep = " ") %>%
        stringr::str_c(volume_number_pages, sep = " ")
      if (!is.null(language)) {
        out <- out %>%
          stringr::str_c(language, sep = " ")
      }
      out <- out %>%
        stringr::str_c(button(item), sep = ". ")
    }
    else {
      if (type_ == c("presentation")) {
        out <- title %>%
          stringr::str_c(stringr::str_glue(" (with {authors}).")) %>%
          stringr::str_c(venue, sep = " ") %>%
          stringr::str_c(city, sep = ": ") %>%
          stringr::str_c(country, sep = ", ") %>%
          stringr::str_c(date_, sep = ", ") %>%
          stringr::str_c(stringr::str_remove_all(year, "[()]"), sep = ", ") %>%
          stringr::str_c(button(item), sep = ". ")
      }
      else {
        if (type_ == "proceeding") {
          out <- authors %>%
            stringr::str_c(year, sep = " ") %>%
            stringr::str_c(title, sep = " ") %>%
            stringr::str_c(venue, sep = " ") %>%
            stringr::str_c(pages, sep = ": ") %>%
            stringr::str_c(city, sep = ". ") %>%
            stringr::str_c(country, sep = ", ") %>%
            stringr::str_c(button(item), sep = ". ")
        }
        else {
          if (type_ == "chapter") {
            out <- authors %>%
              stringr::str_c(year, sep = " ") %>%
              stringr::str_c(title, sep = " ") %>%
              stringr::str_c(stringr::str_glue('in _{book}_'), sep = " ") %>%
              stringr::str_c(stringr::str_glue('chapter {chapter}'), sep = ", ") %>%
              stringr::str_c(pages, sep = ": ") %>%
              stringr::str_c(publisher, sep = ", ") %>%
              stringr::str_c(button(item), sep = ". ")
          }
        }
      }
    }
  }
  return(out)
}


publications_qmd <- c()

qmd_header <- c(
  '---',
  'title: "Publications (2004-2024)"',
  'toc-location: left',
  'toc-title: "Contents"',
  '---',
  ''
)

publications_qmd <- append(publications_qmd, qmd_header)

publications <- yaml::read_yaml(here::here("_publications.yml"))

# adding doctoral thesis
doctoral_thesis <- publications$doctoral_thesis
year <- doctoral_thesis$ryeznik2019phd$year

publications_qmd <- publications_qmd %>%
  append(c('## Doctoral thesis', '')) %>%
  append(c(stringr::str_glue('### {year}', ''))) %>%
  append(c(format_publication(doctoral_thesis$ryeznik2019phd, "thesis"), ''))


# adding preprints
preprints <- publications$preprints
years <- purrr::map_dbl(preprints, ~ .$year) %>%
  unique()

publications_qmd <- publications_qmd %>%
  append(c('## Preprints', ''))

for (year in years) {
  items <- purrr::map(
    preprints,
    ~ if(.$year == year) .x
  ) %>%
    purrr::discard(is.null)
  
  publications_qmd <- publications_qmd %>%
    append(c(stringr::str_glue('### {year}', '')))
  for (item in items) {
    publications_qmd <- publications_qmd %>%
      append(c(format_publication(item, "paper"), ''))
  }
}


# adding peer-reviewed publications
peer_reviewed_publications <- publications$peer_reviewed
years <- purrr::map_dbl(peer_reviewed_publications, ~ .$year) %>%
  unique()

publications_qmd <- publications_qmd %>%
  append(c('## Peer-reviewed publications', ''))

for (year in years) {
  items <- purrr::map(
    peer_reviewed_publications,
    ~ if(.$year == year) .x
  ) %>%
    purrr::discard(is.null)

  publications_qmd <- publications_qmd %>%
    append(c(stringr::str_glue('### {year}', '')))
  for (item in items) {
    publications_qmd <- publications_qmd %>%
      append(c(format_publication(item, "paper"), ''))
  }
}


# adding books, book chapters, tutorials
books_tutorials <- publications$books_tutorials
years <- purrr::map_dbl(books_tutorials, ~ .$year) %>%
  unique()

publications_qmd <- publications_qmd %>%
  append(c('## Books, book chapters, and tutorials', ''))

for (year in years) {
  items <- purrr::map(
    books_tutorials,
    ~ if(.$year == year) .x
  ) %>%
    purrr::discard(is.null)

  publications_qmd <- publications_qmd %>%
    append(c(stringr::str_glue('### {year}', '')))
  for (item in items) {
    publications_qmd <- publications_qmd %>%
      append(c(format_publication(item, "chapter"), ''))
  }
}

# adding conference presentations
conference_presentations <- publications$conference_presentations
years <- purrr::map_dbl(conference_presentations, ~ .$year) %>%
  unique()

publications_qmd <- publications_qmd %>%
  append(c('## Conference presentations', ''))

for (year in years) {
  items <- purrr::map(
    conference_presentations,
    ~ if(.$year == year) .x
  ) %>%
    purrr::discard(is.null)

  publications_qmd <- publications_qmd %>%
    append(c(stringr::str_glue('### {year}', '')))
  for (item in items) {
    publications_qmd <- publications_qmd %>%
      append(c(format_publication(item, "presentation"), ''))
  }
}


# adding conference proceedings
conference_proceedings <- publications$conference_proceedings
years <- purrr::map_dbl(conference_proceedings, ~ .$year) %>%
  unique()

publications_qmd <- publications_qmd %>%
  append(c('## Conference proceedings', ''))

for (year in years) {
  items <- purrr::map(
    conference_proceedings,
    ~ if(.$year == year) .x
  ) %>%
    purrr::discard(is.null)

  publications_qmd <- publications_qmd %>%
    append(c(stringr::str_glue('### {year}', '')))
  for (item in items) {
    publications_qmd <- publications_qmd %>%
      append(c(format_publication(item, "proceeding"), ''))
  }
}

writeLines(publications_qmd, here::here("publications.qmd"))

