(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 receive)
             (ice-9 regex)
             (oop goops)
             (tests common)
             (smc context char)
             (smc core transition)
             (smc cli command-profile))


(define %test-name "profiler")

(configure-test-logging! %test-name)
(test-begin %test-name)



(define %test-data
  (string-join
   (list
    "2023-04-22 23:00:09.032625 (DEBUG): [*] -> [read]"
    "2023-04-22 23:00:09.032748 (DEBUG): [read] -> [read_section_title]"
    "2023-04-22 23:00:09.033559 (DEBUG): [read_section_title] -> [read_section_content]"
    "2023-04-22 23:00:09.033613 (DEBUG): [read_section_content] -> [read_section_property_key]"
    "2023-04-22 23:00:09.033675 (DEBUG): [read_section_property_key] -> [trim_section_property_key]"
    "2023-04-22 23:00:09.033717 (DEBUG): [trim_section_property_key] -> [trim_section_property_value]"
    "2023-04-22 23:00:09.033754 (DEBUG): [trim_section_property_value] -> [read_section_property_value]"
    "2023-04-22 23:00:09.033811 (DEBUG): [read_section_property_value] -> [read_section_content]"
    "2023-04-22 23:00:09.033840 (DEBUG): [read_section_content] -> [read_section_property_key]"
    "2023-04-22 23:00:09.033922 (DEBUG): [read_section_property_key] -> [trim_section_property_key]"
    "2023-04-22 23:00:09.034378 (DEBUG): [trim_section_property_key] -> [trim_section_property_value]"
    "2023-04-22 23:00:09.034415 (DEBUG): [trim_section_property_value] -> [read_section_property_value]"
    "2023-04-22 23:00:09.034450 (DEBUG): [read_section_property_value] -> [read_section_content]"
    "2023-04-22 23:00:09.034478 (DEBUG): [read_section_content] -> [read_section_property_key]"
    "2023-04-22 23:00:09.034522 (DEBUG): [read_section_property_key] -> [trim_section_property_key]"
    "2023-04-22 23:00:09.034547 (DEBUG): [trim_section_property_key] -> [trim_section_property_value]"
    "2023-04-22 23:00:09.034574 (DEBUG): [trim_section_property_value] -> [read_section_property_value]"
    "2023-04-22 23:00:09.034661 (DEBUG): [read_section_property_value] -> [read_section_content]"
    "2023-04-22 23:00:09.034700 (DEBUG): [read_section_content] -> [read_section_property_key]"
    "2023-04-22 23:00:09.034738 (DEBUG): [read_section_property_key] -> [trim_section_property_key]"
    "2023-04-22 23:00:09.034765 (DEBUG): [trim_section_property_key] -> [trim_section_property_value]"
    "2023-04-22 23:00:09.034790 (DEBUG): [trim_section_property_value] -> [read_section_property_value]"
    "2023-04-22 23:00:09.034830 (DEBUG): [read_section_property_value] -> [read_section_content]"
    "2023-04-22 23:00:09.034889 (DEBUG): [read_section_content] -> [*]")
   "\n"))


(test-assert "command-profile: human-readable output"
  (with-input-from-string
      %test-data
    (lambda ()
      (with-output-to-string
        (lambda ()
          (command-profile '("smc" "--log-driver" "null")))))))

(test-assert "command-profile: json output"
  (with-input-from-string
      %test-data
    (lambda ()
      (with-output-to-string
        (lambda ()
          (command-profile '("smc" "--log-driver" "null" "--format" "json")))))))



(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
