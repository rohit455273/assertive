# From assertive.reflection is-os.R, assert-is-os.R, is-32-64-bit.R, assert-is-32-64-bit.R

#' What OS is running?
#' 
#' See \code{\link[assertive.reflection]{is_windows}}.
#' @name is_windows
#' @aliases is_bsd is_linux is_mac is_solaris is_unix is_32_bit is_64_bit assert_is_windows assert_is_bsd assert_is_linux assert_is_mac assert_is_solaris assert_is_unix assert_is_32_bit assert_is_64_bit
#' @export is_windows
#' @export is_bsd
#' @export is_linux
#' @export is_mac
#' @export is_solaris
#' @export is_unix
#' @export is_32_bit
#' @export is_64_bit
#' @export assert_is_windows
#' @export assert_is_bsd
#' @export assert_is_linux
#' @export assert_is_mac
#' @export assert_is_solaris
#' @export assert_is_unix
#' @export assert_is_32_bit
#' @export assert_is_64_bit
NULL

# From assertive.reflection is-current.R, assert-is-current.R

#' Is this version of R up to date?
#' 
#' See \code{\link[assertive.reflection]{}}.
#' @name is_r_current
#' @aliases assert_is_r_current
#' @export is_r_current
#' @export assert_is_r_current
NULL

#' Is the installed version of a package current?
#' 
#' See \code{\link[assertive.reflection]{is_package_current}}.
#' @name is_package_current
#' @aliases assert_is_package_current
#' @export is_package_current
#' @export assert_is_package_current
NULL

# From assertive.reflection is-decimal-point.R, assert-is-decimal-point.R

#' What does the current locale specify for the decimal point?
#' 
#' See \code{\link[assertive.reflection]{is_xxx_for_decimal_point}}.
#' @name is_xxx_for_decimal_point
#' @aliases is_comma_for_decimal_point is_comma_for_decimal_point assert_is_comma_for_decimal_point assert_is_period_for_decimal_point
#' @export is_comma_for_decimal_point
#' @export is_period_for_decimal_point
#' @export assert_is_comma_for_decimal_point
#' @export assert_is_period_for_decimal_point
NULL

# From assertive.reflection is-r-mode.R, assert-is-r-mode.R, is-ide.R, assert-is-ide.R, is-r-version.R, assert-is-r-version.R

#' Are you running R?
#' 
#' See \code{\link[assertive.reflection]{is_r}}.
#' @name is_r
#' @aliases is_architect is_revo_r is_rstudio is_r_alpha is_r_beta is_r_devel is_r_patched is_r_release_candidate is_r_release assert_is_r assert_is_architect assert_is_revo_r assert_is_rstudio assert_is_r_alpha assert_is_r_beta assert_is_r_devel assert_is_r_patched assert_is_r_release_candidate  assert_is_r_release
#' @export is_r
#' @export is_architect
#' @export is_revo_r
#' @export is_rstudio
#' @export is_r_alpha
#' @export is_r_beta
#' @export is_r_devel
#' @export is_r_patched
#' @export is_r_release_candidate
#' @export is_r_release
#' @export assert_is_r
#' @export assert_is_architect
#' @export assert_is_revo_r
#' @export assert_is_rstudio
#' @export assert_is_r_alpha
#' @export assert_is_r_beta
#' @export assert_is_r_devel
#' @export assert_is_r_patched
#' @export assert_is_r_release_candidate
#' @export assert_is_r_release
NULL

#' How is R running?
#' 
#' See \code{\link[assertive.reflection]{is_batch_mode}}.
#' @name is_batch_mode
#' @aliases is_interactive is_r_slave assert_is_batch_mode assert_is_interactive assert_
#' @export is_batch_mode
#' @export is_interactive
#' @export is_r_slave
#' @export assert_is_batch_mode
#' @export assert_is_interactive
#' @export assert_is_r_slave
NULL

#'Is RStudio the current version?
#' 
#' See \code{\link[assertive.reflection]{is_rstudio_current}}.
#' @name is_rstudio_current
#' @aliases assert_is_rstudio_current
#' @export is_rstudio_current
#' @export assert_is_rstudio_current
NULL

#' Is RStudio running in desktop or server mode?
#' 
#' See \code{\link[assertive.reflection]{is_rstudio_desktop}}.
#' @name is_rstudio_desktop
#' @aliases is_rstudio_server assert_is_rstudio_desktop assert_is_rstudio_server
#' @export is_rstudio_desktop
#' @export is_rstudio_server
#' @export assert_is_rstudio_desktop
#' @export assert_is_rstudio_server
NULL

# From assertive.reflection is-on-os-path.R, assert-is-on-os-path.R

#' Is the path on the OS path?
#' 
#' See \code{\link[assertive.reflection]{is_on_os_path}}.
#' @name is_on_os_path
#' @aliases assert_all_are_on_os_path assert_any_are_on_os_path
#' @export is_on_os_path
#' @export assert_all_are_on_os_path
#' @export assert_any_are_on_os_path
NULL

# From assertive.reflection r-has-capability.R, assert-r-has-capability-.R

#'Can R find tools?
#' 
#' See \code{\link[assertive.reflection]{r_can_find_tools}}.
#' @name r_can_find_tools 
#' @aliases  r_can_compile_code r_can_build_translations assert_r_can_find_tools assert_r_can_compile_code assert_r_can_build_translations
#' @export r_can_find_tools
#' @export r_can_compile_code
#' @export r_can_build_translations
#' @export assert_r_can_find_tools
#' @export assert_r_can_compile_code
#' @export assert_r_can_build_translations
NULL

#' Does R have a capability?
#' 
#' See \code{\link[assertive.reflection]{r_has_jpeg_capability}}.
#' @name r_has_jpeg_capability
#' @aliases r_has_png_capability r_has_tiff_capability r_has_tcltk_capability r_has_x11_capability r_has_aqua_capability r_has_http_ftp_capability r_has_sockets_capability r_has_libxml_capability r_has_fifo_capability r_has_cledit_capability r_has_iconv_capability r_has_nls_capability r_has_profmem_capability r_has_cairo_capability r_has_icu_capability r_has_long_double_capability r_has_libcurl_capability assert_r_has_jpeg_capability assert_r_has_png_capability assert_r_has_tiff_capability assert_r_has_tcltk_capability assert_r_has_x11_capability assert_r_has_aqua_capability assert_r_has_http_ftp_capability assert_r_has_sockets_capability assert_r_has_libxml_capability assert_r_has_fifo_capability assert_r_has_cledit_capability assert_r_has_iconv_capability assert_r_has_nls_capability assert_r_has_profmem_capability assert_r_has_cairo_capability assert_r_has_icu_capability assert_r_has_long_double_capability assert_r_has_libcurl_capability
#' @export r_has_jpeg_capability
#' @export r_has_png_capability
#' @export r_has_tiff_capability
#' @export r_has_tcltk_capability
#' @export r_has_x11_capability
#' @export r_has_aqua_capability
#' @export r_has_http_ftp_capability
#' @export r_has_sockets_capability
#' @export r_has_libxml_capability
#' @export r_has_fifo_capability
#' @export r_has_cledit_capability
#' @export r_has_iconv_capability
#' @export r_has_nls_capability
#' @export r_has_profmem_capability
#' @export r_has_cairo_capability
#' @export r_has_icu_capability
#' @export r_has_long_double_capability
#' @export r_has_libcurl_capability
#' @export assert_r_has_jpeg_capability
#' @export assert_r_has_png_capability
#' @export assert_r_has_tiff_capability
#' @export assert_r_has_tcltk_capability
#' @export assert_r_has_x11_capability
#' @export assert_r_has_aqua_capability
#' @export assert_r_has_http_ftp_capability
#' @export assert_r_has_sockets_capability
#' @export assert_r_has_libxml_capability
#' @export assert_r_has_fifo_capability
#' @export assert_r_has_cledit_capability
#' @export assert_r_has_iconv_capability
#' @export assert_r_has_nls_capability
#' @export assert_r_has_profmem_capability
#' @export assert_r_has_cairo_capability
#' @export assert_r_has_icu_capability
#' @export assert_r_has_long_double_capability
#' @export assert_r_has_libcurl_capability
NULL

# From assertive.reflection locale.R

#' Get or set the system locale
#' 
#' See \code{\link[assertive.reflection]{sys_get_locale}}.
#' @name sys_get_locale
#' @aliases sys_set_locale
#' @export sys_get_locale
#' @export sys_set_locale
NULL
