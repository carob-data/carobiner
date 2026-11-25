
.valid_ymd <- function(yr, mon, day, min_year, max_year) {
	if (any(is.na(c(yr, mon, day)))) return(NA_character_)
	if (yr < min_year || yr > max_year || mon < 1L || mon > 12L || day < 1L || day > 31L) {
		return(NA_character_)
	}
	s <- sprintf("%04d-%02d-%02d", yr, mon, day)
	d <- as.Date(s, format = "%Y-%m-%d")
	if (is.na(d) || format(d, "%Y-%m-%d") != s) return(NA_character_)
	s
}


.expand_year <- function(s, min_year, max_year) {
	if (grepl("^[0-9]{4}$", s)) {
		y <- as.integer(s)
	} else if (grepl("^[0-9]{2}$", s)) {
		y <- 2000L + as.integer(s)
		if (y > max_year) y <- 1900L + as.integer(s)
	} else {
		return(NA_integer_)
	}
	if (y < min_year || y > max_year) return(NA_integer_)
	y
}


.month_num <- function(s) {
	s <- tolower(sub("\\.$", "", s))
	s <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00e0\u00e8\u00ea\u00fb\u00f4", "aeiouunaeeuo", s)
	map <- c(
		jan = 1L, january = 1L, ene = 1L, enero = 1L, janv = 1L, janvier = 1L,
		feb = 2L, february = 2L, febrero = 2L, fev = 2L, fevr = 2L, fevrier = 2L,
		mar = 3L, march = 3L, marzo = 3L, mars = 3L,
		apr = 4L, april = 4L, abr = 4L, abril = 4L, avr = 4L, avril = 4L,
		may = 5L, mayo = 5L, mai = 5L,
		jun = 6L, june = 6L, junio = 6L, juin = 6L,
		jul = 7L, july = 7L, julio = 7L, juil = 7L, juill = 7L, juillet = 7L,
		aug = 8L, august = 8L, ago = 8L, agosto = 8L, aout = 8L,
		sep = 9L, sept = 9L, september = 9L, set = 9L, setiembre = 9L, septiembre = 9L, septembre = 9L,
		oct = 10L, october = 10L, octubre = 10L, octobre = 10L,
		nov = 11L, november = 11L, noviembre = 11L, novembre = 11L,
		dec = 12L, december = 12L, dic = 12L, diciembre = 12L, decembre = 12L
	)
	unname(map[s])
}


parse_dates <- function(x, order = c("auto", "dmy", "mdy", "ymd"),
		ambiguous = c("na", "dmy", "mdy"),
		min_year = 1950L, max_year = 2035L,
		origin = "1899-12-30", partial = FALSE,
		na = c("", "-", "\u2013", "NA", "N/A", "n/a", "#N/A", ".", "NULL")) {

	order <- match.arg(order)
	ambiguous <- match.arg(ambiguous)
	min_year <- as.integer(min_year)
	max_year <- as.integer(max_year)

	if (is.null(x) || length(x) == 0L) return(character(0))

	if (inherits(x, "Date") || inherits(x, "POSIXt")) {
		out <- as.character(as.Date(x))
		ok <- !is.na(out)
		if (any(ok)) {
			yr <- as.integer(substr(out[ok], 1L, 4L))
			bad <- is.na(yr) | yr < min_year | yr > max_year
			if (any(bad)) out[ok][bad] <- NA_character_
		}
		return(out)
	}

	ch <- trimws(gsub("[\r\n\t]+", " ", as.character(x)))
	ch[is.na(ch) | ch %in% na] <- NA_character_
	out <- rep(NA_character_, length(ch))

	# 1. Numeric values: Excel serials, Unix timestamps, and optional 4-digit years
	is_num <- !is.na(ch) & grepl("^[0-9]+(\\.[0-9]+)?$", ch)
	if (any(is_num)) {
		idx <- which(is_num)
		vals <- as.numeric(ch[idx])
		for (k in seq_along(idx)) {
			i <- idx[k]
			v <- vals[k]
			if (isTRUE(partial) && grepl("^[0-9]{4}$", ch[i])) {
				yr <- as.integer(ch[i])
				if (yr >= min_year && yr <= max_year) {
					out[i] <- ch[i]
					next
				}
			}
			d <- as.Date(NA)
			if (v > 0 && v < 100000) {
				d <- as.Date(floor(v), origin = origin)
			} else if (v >= 1e8 && v < 4e9) {
				d <- as.Date(as.POSIXct(v, origin = "1970-01-01", tz = "UTC"))
			}
			if (!is.na(d)) {
				s <- as.character(d)
				yr <- as.integer(substr(s, 1L, 4L))
				if (!is.na(yr) && yr >= min_year && yr <= max_year) {
					out[i] <- s
				}
			}
		}
	}

	# 2. ISO / YMD dates and datetimes (YYYY-MM-DD, YYYY/MM/DD, YYYY.MM.DD)
	rem <- is.na(out) & !is.na(ch)
	if (any(rem)) {
		pat_ymd <- "^([0-9]{4})([-/.])([0-9]{1,2})\\2([0-9]{1,2})(?:[T ][0-9]{1,2}:[0-9]{2}.*)?$"
		hit <- which(rem & grepl(pat_ymd, ch))
		for (i in hit) {
			s <- sub("[T ][0-9]{1,2}:[0-9]{2}.*$", "", ch[i])
			p <- strsplit(s, "[-/.]")[[1]]
			out[i] <- .valid_ymd(as.integer(p[1]), as.integer(p[2]), as.integer(p[3]), min_year, max_year)
		}
	}
	if (isTRUE(partial)) {
		rem <- is.na(out) & !is.na(ch)
		hit <- which(rem & grepl("^[0-9]{4}[-/][0-9]{1,2}$", ch))
		for (i in hit) {
			p <- strsplit(ch[i], "[-/]")[[1]]
			yr <- as.integer(p[1])
			mon <- as.integer(p[2])
			if (yr >= min_year && yr <= max_year && mon >= 1L && mon <= 12L) {
				out[i] <- sprintf("%04d-%02d", yr, mon)
			}
		}
	}

	# 3. Named-month dates (English, Spanish, French): D-Mon-Y, Mon D, Y, Y-Mon-D
	rem <- is.na(out) & !is.na(ch) & grepl("[A-Za-z]", ch)
	if (any(rem)) {
		for (i in which(rem)) {
			s <- sub("\\s+[0-9]{1,2}:[0-9]{2}.*$", "", ch[i])
			s <- trimws(gsub(",", " ", s))
			p <- strsplit(s, "[-/. ]+")[[1]]
			p <- p[nzchar(p)]
			if (length(p) != 3L) next
			m1 <- .month_num(p[1])
			m2 <- .month_num(p[2])
			if (!is.na(m2) && grepl("^[0-9]{1,2}$", p[1])) {
				yr <- .expand_year(p[3], min_year, max_year)
				out[i] <- .valid_ymd(yr, m2, as.integer(p[1]), min_year, max_year)
			} else if (!is.na(m1) && grepl("^[0-9]{1,2}$", p[2])) {
				yr <- .expand_year(p[3], min_year, max_year)
				out[i] <- .valid_ymd(yr, m1, as.integer(p[2]), min_year, max_year)
			} else if (!is.na(m2) && grepl("^[0-9]{4}$", p[1]) && grepl("^[0-9]{1,2}$", p[3])) {
				out[i] <- .valid_ymd(as.integer(p[1]), m2, as.integer(p[3]), min_year, max_year)
			}
		}
	}

	# 4. Numeric 3-part dates (d/m/y, m/d/y, or 2-digit y/m/d when order="ymd")
	rem <- is.na(out) & !is.na(ch)
	if (any(rem)) {
		s_clean <- sub("\\s+[0-9]{1,2}:[0-9]{2}.*$", "", ch)
		pat_num3 <- "^([0-9]{1,2})([-/.])([0-9]{1,2})\\2([0-9]{2}|[0-9]{4})$"
		hit <- which(rem & grepl(pat_num3, s_clean))
		if (length(hit) > 0L) {
			a <- integer(length(hit))
			b <- integer(length(hit))
			c_str <- character(length(hit))
			for (k in seq_along(hit)) {
				p <- strsplit(s_clean[hit[k]], "[-/.]")[[1]]
				a[k] <- as.integer(p[1])
				b[k] <- as.integer(p[2])
				c_str[k] <- p[3]
			}
			if (order == "ymd") {
				for (k in seq_along(hit)) {
					yr <- .expand_year(sprintf("%02d", a[k]), min_year, max_year)
					if (nchar(c_str[k]) <= 2L) {
						out[hit[k]] <- .valid_ymd(yr, b[k], as.integer(c_str[k]), min_year, max_year)
					}
				}
			} else {
				eff <- order
				if (eff == "auto") {
					has_a <- any(a > 12L)
					has_b <- any(b > 12L)
					if (has_a && !has_b) {
						eff <- "dmy"
					} else if (has_b && !has_a) {
						eff <- "mdy"
					}
				}
				for (k in seq_along(hit)) {
					yr <- .expand_year(c_str[k], min_year, max_year)
					if (is.na(yr)) next
					ak <- a[k]
					bk <- b[k]
					if (eff == "dmy") {
						out[hit[k]] <- .valid_ymd(yr, bk, ak, min_year, max_year)
					} else if (eff == "mdy") {
						out[hit[k]] <- .valid_ymd(yr, ak, bk, min_year, max_year)
					} else {
						# auto with no vector-wide resolution (all <= 12 or mixed > 12)
						if (ak > 12L && bk <= 12L) {
							out[hit[k]] <- .valid_ymd(yr, bk, ak, min_year, max_year)
						} else if (bk > 12L && ak <= 12L) {
							out[hit[k]] <- .valid_ymd(yr, ak, bk, min_year, max_year)
						} else if (ak == bk) {
							out[hit[k]] <- .valid_ymd(yr, ak, bk, min_year, max_year)
						} else if (ambiguous == "dmy") {
							out[hit[k]] <- .valid_ymd(yr, bk, ak, min_year, max_year)
						} else if (ambiguous == "mdy") {
							out[hit[k]] <- .valid_ymd(yr, ak, bk, min_year, max_year)
						}
					}
				}
			}
		}
	}

	out
}
