
is_excel <- function(f) {
	if (length(f) != 1L || is.na(f) || !nzchar(f) || !file.exists(f)) {
		return(FALSE)
	}
	v <- readBin(f, "raw", n=4)
	length(v) == 4L && all(as.numeric(v) == c(80, 75, 3, 4))
}


is_biff2 <- function(f) {
	if (length(f) != 1L || is.na(f) || !nzchar(f) || !file.exists(f)) {
		return(FALSE)
	}
	v <- readBin(f, "raw", n=4)
	length(v) == 4L && all(v[1:2] == as.raw(c(0x09, 0x00)))
}


# Excel 2.1 (BIFF2) worksheet reader: INTEGER, NUMBER, LABEL, BOOLERR and FORMULA cell records
read.biff2 <- function(f, col_names=TRUE, skip=0L, na="", fix_names=FALSE, lower=FALSE) {
	b <- readBin(f, "raw", file.size(f))
	bi <- as.integer(b)
	n <- length(bi)
	pos <- integer(max(16L, n %/% 4L))
	ids <- integer(max(16L, n %/% 4L))
	p <- 1L
	k <- 0L
	while (p + 3L <= n) {
		id <- bi[p] + 256L * bi[p + 1L]
		k <- k + 1L
		if (k > length(pos)) {
			pos <- c(pos, integer(length(pos)))
			ids <- c(ids, integer(length(ids)))
		}
		pos[k] <- p + 4L
		ids[k] <- id
		if (id == 10L) break
		p <- p + 4L + bi[p + 2L] + 256L * bi[p + 3L]
	}
	cell <- ids[seq_len(k)] %in% 2:6
	pos <- pos[seq_len(k)][cell]
	ids <- ids[seq_len(k)][cell]
	if (length(pos) == 0L) {
		return(data.frame())
	}
	u16 <- function(i) bi[i] + 256L * bi[i + 1L]
	rows <- u16(pos) + 1L
	cols <- u16(pos + 2L) + 1L
	num <- rep(NA_real_, length(pos))
	chr <- rep(NA_character_, length(pos))
	i <- ids == 2L
	if (any(i)) {
		num[i] <- u16(pos[i] + 7L)
	}
	i <- which(ids %in% c(3L, 6L))
	if (length(i) > 0L) {
		idx <- as.vector(outer(0:7, pos[i] + 7L, "+"))
		num[i] <- readBin(b[idx], "double", n=length(i), size=8, endian="little")
	}
	for (j in which(ids == 4L)) {
		nc <- bi[pos[j] + 7L]
		chr[j] <- if (nc > 0L) rawToChar(b[(pos[j] + 8L):(pos[j] + 7L + nc)]) else ""
	}
	i <- which(ids == 5L)
	if (length(i) > 0L) {
		is_err <- bi[pos[i] + 8L]
		val <- bi[pos[i] + 7L]
		ok <- is_err == 0L
		if (any(ok)) {
			num[i[ok]] <- val[ok]
		}
	}
	skip <- as.integer(skip)
	nr <- max(rows)
	nc <- max(cols)
	if (isTRUE(col_names)) {
		hdr_row <- skip + 1L
		h <- rows == hdr_row
		hdr <- paste0("V", seq_len(nc))
		hval <- ifelse(is.na(chr[h]), as.character(num[h]), chr[h])
		hdr[cols[h]] <- hval
		data_start <- hdr_row
	} else {
		hdr <- paste0("V", seq_len(nc))
		data_start <- skip
	}
	if (length(na) > 0L) {
		chr[chr %in% na] <- NA_character_
	}
	n_out <- max(0L, nr - data_start)
	out <- lapply(seq_len(nc), function(j) {
		i <- which(cols == j & rows > data_start)
		if (all(is.na(chr[i]))) {
			v <- rep(NA_real_, n_out)
			v[rows[i] - data_start] <- num[i]
		} else {
			v <- rep(NA_character_, n_out)
			v[rows[i] - data_start] <- ifelse(is.na(chr[i]), as.character(num[i]), chr[i])
		}
		v
	})
	names(out) <- hdr
	x <- as.data.frame(out, check.names=FALSE)
	if (fix_names) {
		colnames(x) <- fix_varnames(colnames(x))
	}
	if (lower) {
		colnames(x) <- tolower(colnames(x))
	}
	x
}


read.excel <- function(f, fix_names=FALSE, lower=FALSE, ...) {
	if (length(f) != 1L || is.na(f) || !nzchar(f)) {
		stop("f must be a single file path (got length ", length(f), ")", call. = FALSE)
	}
	if (!file.exists(f)) {
		stop("file does not exist: ", f, call. = FALSE)
	}
	x <- tryCatch(
		suppressMessages(as.data.frame(readxl::read_excel(f, ...))),
		error = function(e) {
			if (is_excel(f) && !grepl("\\.xlsx$", f, ignore.case = TRUE)) {
				suppressMessages(as.data.frame(readxl::read_xlsx(f, ...)))
			} else if (is_biff2(f)) {
				dots <- list(...)
				read.biff2(
					f,
					col_names = if (is.null(dots$col_names)) TRUE else dots$col_names,
					skip = if (is.null(dots$skip)) 0L else dots$skip,
					na = if (is.null(dots$na)) "" else dots$na
				)
			} else {
				stop(e)
			}
		}
	)
	if (fix_names) {
		colnames(x) <- fix_varnames(colnames(x))
	}
	if (lower) {
		colnames(x) <- tolower(colnames(x))
	}
	x
}

read.excel.hdr <- function(f, skip, hdr=1, fix_names=TRUE, lower=FALSE, ...) {
	if (length(f) != 1L || is.na(f) || !nzchar(f)) {
		stop("f must be a single file path (got length ", length(f), ")", call. = FALSE)
	}
	if (!file.exists(f)) {
		stop("file does not exist: ", f, call. = FALSE)
	}
	skip <- as.integer(skip)
	hdr <- as.integer(hdr)
	if ((skip==0) && (hdr==1)) {
		return(read.excel(f, fix_names=fix_names, ...))
	}
	stopifnot(skip >= 0)
	stopifnot(hdr >= 1)
	stopifnot(skip >= hdr)
	reader <- if (is_excel(f) && !grepl("\\.xlsx$", f, ignore.case = TRUE)) readxl::read_xlsx else readxl::read_excel
	suppressMessages(x <- as.data.frame(reader(f, skip=skip, ...)))
	suppressMessages(nms <- as.data.frame(reader(f, skip=skip-hdr, n_max=hdr+1, col_names=FALSE, ...)))
	nms <- apply(nms, 2, function(i) paste(i[!is.na(i)], collapse="_"))
	if (fix_names) {
		nms <- fix_varnames(nms)
	}
	if (lower) {
		nms <- tolower(nms)
	}
	colnames(x) <- nms[1:ncol(x)]
	x
}



excel_textbox <- function(filename) {
	if (!grepl("\\.xlsx$", filename)) {
		stop("this only works for xlsx files")
	}
	xpath <- file.path(tempdir(), "texel", basename(filename))
	dir.create(xpath, FALSE, TRUE)
	utils::unzip(filename, exdir=xpath)
	fdraw <- list.files(file.path(xpath, "xl", "drawings"), pattern = "\\.xml$", full.names=TRUE)
	out <- sapply(fdraw, function(f) {
		xml <- xml2::read_xml(f)
		txt <- xml2::xml_text(xml, trim = TRUE)
		gsub("\\d{12,200}", "", txt)
	})
	unlink(xpath)
	out
}

