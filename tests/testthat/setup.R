# No progress bars in tests. With an active progress handler (an interactive
# session), progressr re-signals errors raised inside with_progress() in a way
# expect_error() cannot catch, so devtools::test() in the console would fail
# while a headless run passes.
progressr::handlers("void")
