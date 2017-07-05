TEST_ARCHIVE=gcc_dg_pre.tar.bz2
TEST_SUITE=gcc_pre
TMP_DIR=gcc_pre_tmp
export CTEST_TMPDIR=$(TMP_DIR)
export CTEST_EXIT_FAILURE=1

all: expect_parse expect_fail

full: all expect_roundtrip memory_usage_parse.out memory_usage_roundtrip.out

# parse tests
EXPECT_PARSE_TARGETS = $(addprefix parse_, $(shell cat expect_parse.txt))
EXPECT_FAIL_TARGETS = $(addprefix fail_, $(shell cat expect_fail.txt))

expect_parse: $(EXPECT_PARSE_TARGETS)
expect_fail:  $(EXPECT_FAIL_TARGETS)

$(EXPECT_PARSE_TARGETS): parse_%: $(TEST_SUITE)
	@rm -f $(TMP_DIR)/$@.log
	@CTEST_LOGFILE=$(TMP_DIR)/$@.log ../../bin/CParse +RTS -t_stats.log -RTS \
	  $(TEST_SUITE)/$(patsubst parse_%,%,$@) \
	  || (cat $(TMP_DIR)/$@.log >&2 ; exit 1)
	@cat _stats.log >> $(TMP_DIR)/$@.log

$(EXPECT_FAIL_TARGETS): fail_%: $(TEST_SUITE)
	@rm -f $(TMP_DIR)/$@.log
	@CTEST_NON_PARSE=1 CTEST_LOGFILE=$(TMP_DIR)/$@.log ../../bin/CParse +RTS -t_stats.log -RTS \
	  $(TEST_SUITE)/$(patsubst fail_%,%,$@) \
	  || (cat $(TMP_DIR)/$@.log >&2 ; exit 1)
	@cat _stats.log >> $(TMP_DIR)/@.log


# More tests and stats

EXPECT_ROUNDTRIP_TARGETS = $(addprefix roundtrip_, $(shell cat expect_roundtrip.txt))
expect_roundtrip: $(EXPECT_ROUNDTRIP_TARGETS)

$(EXPECT_ROUNDTRIP_TARGETS): roundtrip_%: $(TEST_SUITE)
	@rm -f $(TMP_DIR)/$@.log
	@CTEST_LOGFILE=$(TMP_DIR)/$@.log ../../bin/CRoundTrip +RTS -t_stats.log -RTS \
	  $(TEST_SUITE)/$(patsubst roundtrip_%,%,$@) \
	  || (cat $(TMP_DIR)/$@.log >&2 ; exit 1)
	@cat _stats.log >> $(TMP_DIR)/$@.log

memory_usage_parse.out: $(TEST_SUITE)
	time for f in `cat expect_parse.txt` ; do \
	  CTEST_TMP_DIR=$(TMP_DIR) ../../bin/CParse +RTS -t -RTS $(TEST_SUITE)/$$f 2>&1 | \
	  grep -o [1-9][0-9]*M | sed 's/^/'"$$f"': /' ; \
        done > $@

memory_usage_roundtrip.out: $(TEST_SUITE)
	time for f in `cat expect_roundtrip.txt` ; do \
	  CTEST_TMP_DIR=$(TMP_DIR) ../../bin/CRoundTrip +RTS -t -RTS $(TEST_SUITE)/$$f 2>&1 | \
	  grep -o [1-9][0-9]*M | sed 's/^/'"$$f"': /' ; \
        done > $@


$(TEST_SUITE): $(TMP_DIR)
	@if [ ! -d $(TEST_SUITE) ] ; then tar xjf $(TEST_ARCHIVE) ; fi
$(TMP_DIR):
	@mkdir -p $@

clean:
	rm -rf $(TEST_SUITE) $(TMP_DIR) _stats.log *.out

# generating the expect/fail lists
.PHONY: triage_parse triage_roundtrip
triage_parse:
	@rm -f expect_fail.txt expect_parse.txt
	@touch expect_fail.txt expect_parse.txt
	time for f in `ls $(TEST_SUITE) ` ; do ../../bin/CParse +RTS -t -RTS $(TEST_SUITE)/$$f ; if [ $$? -ne 0 ] ; then echo $$f >> expect_fail.txt ; else echo $$f >> expect_parse.txt ; fi ; done 2>triage.log
triage_roundtrip:
	@rm -f expect_roundtrip.txt expect_parse_only.txt
	@touch expect_roundtrip.txt expect_parse_only.txt
	time for f in `cat expect_parse.txt` ; do ../../bin/CRoundTrip +RTS -t -RTS $(TEST_SUITE)/$$f ; if [ $$? -ne 0 ] ; then echo $$f >> expect_parse_only.txt ; else echo $$f >> expect_roundtrip.txt ; fi ; done 2>triage2.log

