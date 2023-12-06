
#let poster(
  // The poster's size.
  size: "'36x24' or '48x36''",

  // The poster's title.
  title: "Paper Title",

  // A string of author names.
  authors: "Author Names (separated by commas)",

  // Department name.
  departments: "",

  // University logo.
  univ_logo: "Logo Path",

  // Footer text.
  // For instance, Name of Conference, Date, Location.
  // or Course Name, Date, Instructor.
  footer_text: "Footer Text",

  // Any URL, like a link to the conference website.
  footer_url: "Footer URL",

  // Email IDs of the authors.
  footer_email_ids: "Email IDs (separated by commas)",

  // Color of the footer.
  footer_color: "Hex Color Code",

  // DEFAULTS
  // ========
  // For 3-column posters, these are generally good defaults.
  // Tested on 36in x 24in, 48in x 36in, and 36in x 48in posters.
  // For 2-column posters, you may need to tweak these values.
  // See ./examples/example_2_column_18_24.typ for an example.

  // Any keywords or index terms that you want to highlight at the beginning.
  keywords: (),

  // Number of columns in the poster.
  num_columns: "3",

  // University logo's scale (in %).
  univ_logo_scale: "100",

  // University logo's column size (in in).
  univ_logo_column_size: "10",

  // Title and authors' column size (in in).
  title_column_size: "20",

  // Poster title's font size (in pt).
  title_font_size: "56",

  // Authors' font size (in pt).
  authors_font_size: "28",

  // Author's Institution font size
  institution_font_size: "20",

  // Footer's URL and email font size (in pt).
  footer_url_font_size: "20",

  // Footer's text font size (in pt).
  footer_text_font_size: "30",

  


  // The poster's content.
  body
) = {
  // Set the body font.
  set text(font: "STIX Two Text", size: 20pt)
  let sizes = size.split("x")
  let width = int(sizes.at(0)) * 1in
  let height = int(sizes.at(1)) * 1in
  univ_logo_scale = int(univ_logo_scale) * 1%
  title_font_size = int(title_font_size) * 1pt
  authors_font_size = int(authors_font_size) * 1pt
  institution_font_size = int(institution_font_size) * 1pt
  num_columns = int(num_columns)
  univ_logo_column_size = int(univ_logo_column_size) * 1in
  title_column_size = int(title_column_size) * 1in
  footer_url_font_size = int(footer_url_font_size) * 1pt
  footer_text_font_size = int(footer_text_font_size) * 1pt

  // Configure the page.
  // This poster defaults to 36in x 24in.
  set page(
    width: width,
    height: height,
    margin: 
      (top: 0.5in, left: 0.75in, right: 0.75in, bottom: 1.25in),
    footer: [
      #set align(center)
      #set text(32pt)
      #line(length: 100%)
      #v(0pt, weak: true)
      #block(
        fill: none,
        stroke: none,
        width: 100%,
        inset: 20pt,
        radius: 10pt,
        align(center,grid(
      rows: 1,
      columns: (1fr, 1fr, 1fr),
      column-gutter: 00pt,
      row-gutter: 0pt,
      align(left,text(size: footer_url_font_size, footer_url)),
      text(size: footer_text_font_size, smallcaps(footer_text)),
      align(right,text(size: footer_url_font_size, footer_email_ids)),
      )
      ))
    ]
  )

  // Configure equation numbering and spacing.
  set math.equation(numbering: "(1)")
  show math.equation: set block(spacing: 0.65em)

  // Configure lists.
  set enum(indent: 10pt, body-indent: 9pt)
  set list(indent: 10pt, body-indent: 9pt)

  // Configure headings.
  set heading(numbering: "I.A.1.")
  show heading: it => locate(loc => {
    // Find out the final number of the heading counter.
    let levels = counter(heading).at(loc)
    let deepest = if levels != () {
      levels.last()
    } else {
      1
    }

    set text(24pt, weight: 400)
    if it.level == 1 [
      // First-level headings are centered smallcaps.
      #set align(center)
      #set text({ 32pt })
      #show: smallcaps
      #v(50pt, weak: true)
      #it.body
      #v(20pt, weak: true)
      #line(length: 100%)
    ] else if it.level == 2 [
      // Second-level headings are run-ins.
      #v(32pt, weak: true)
      #it.body
      #v(20pt, weak: true)
    ] else [
      // Third level headings are run-ins too, but different.
      #set text(20pt, weight: "bold")
      _#(it.body):_
    ]
  })

  // Arranging the logo, title, authors, and department in the header.
  align(center,
    grid(
      rows: 1,
      column-gutter: 0pt,
      row-gutter: 10pt,
      text(title_font_size, title, weight: "extrabold") + "\n" + 
      grid(
        rows: 2,
        columns: 3,
        column-gutter: 50pt,
        row-gutter: 10pt,
        text(authors_font_size, "Tobias Adrian"), 
        text(authors_font_size, "Matthew DeHaven"),
        text(authors_font_size, "Fernando Duarte"),
        text(institution_font_size, "IMF") + super[1], 
        text(institution_font_size, "Brown"),
        text(institution_font_size, "Brown")
      )
    ) 
  )
  line(length: 100%)
  v(10pt)
  // Start three column mode and configure paragraph properties.
  show: columns.with(num_columns, gutter: 64pt)
  set par(justify: true, first-line-indent: 0em)
  show par: set block(spacing: 0.65em)

  // Display the keywords.
  if keywords != () [
      #set text(24pt, weight: 400)
      #show "Keywords": smallcaps
      *Keywords* --- #keywords.join(", ")
  ]

  // Display the poster's contents.
  body
}