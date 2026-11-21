# Tests for install_python_env() and its `python` argument (module parameter `pythonPath`).
# reticulate is mocked, so nothing is installed. Run from the module folder:
#   testthat::test_file("tests/testthat/test-install_python_env.R")

# Load only install_python_env() from the module file
moduleFile <- if (file.exists("spades_ws3_dataInit.R")) "spades_ws3_dataInit.R" else
  testthat::test_path("..", "..", "spades_ws3_dataInit.R")
exprs <- parse(moduleFile)
isFun <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                  identical(e[[2]], as.name("install_python_env")), logical(1))
eval(exprs[[which(isFun)]])

mockReticulate <- function(venvExists, calls) {
  testthat::local_mocked_bindings(
    install_python = function(...) { calls$install_python <- list(...); "/built/python" },
    virtualenv_exists = function(...) venvExists,
    virtualenv_create = function(...) { calls$virtualenv_create <- list(...); invisible() },
    py_install = function(...) { calls$py_install <- list(...); invisible() },
    use_virtualenv = function(...) { calls$use_virtualenv <- list(...); invisible() },
    .package = "reticulate", .env = parent.frame()
  )
}

test_that("default (python = NA) keeps the old behaviour: install_python(), default starter", {
  calls <- new.env()
  mockReticulate(venvExists = FALSE, calls)
  install_python_env(py_version = "3.12", py_packages = c("ws3"), venv = "r-reticulate")
  expect_equal(calls$install_python, list(version = "3.12"))
  expect_equal(calls$virtualenv_create, list("r-reticulate", packages = "ws3"))  # no `python =`
  expect_equal(calls$use_virtualenv, list("r-reticulate"))
})

test_that("python = NULL behaves like the default", {
  calls <- new.env()
  mockReticulate(venvExists = FALSE, calls)
  install_python_env(py_version = "3.12", py_packages = "ws3", venv = "r-reticulate", python = NULL)
  expect_false(is.null(calls$install_python))
  expect_null(calls$virtualenv_create$python)
})

test_that("python = <path> skips install_python() and builds the virtualenv from that path", {
  calls <- new.env()
  mockReticulate(venvExists = FALSE, calls)
  install_python_env(py_version = "3.12", py_packages = "ws3", venv = "r-reticulate",
                     python = "/opt/python3.12")
  expect_null(calls$install_python)
  expect_equal(calls$virtualenv_create,
               list("r-reticulate", python = "/opt/python3.12", packages = "ws3"))
})

test_that("an existing virtualenv gets py_install() either way", {
  for (py in list(NA, "/opt/python3.12")) {
    calls <- new.env()
    mockReticulate(venvExists = TRUE, calls)
    install_python_env(py_version = "3.12", py_packages = "ws3", venv = "r-reticulate", python = py)
    expect_equal(calls$py_install, list("ws3"))
    expect_null(calls$virtualenv_create)
  }
})
