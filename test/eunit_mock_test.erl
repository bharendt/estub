
-include_lib("include/eunit.hrl").


apply_parsetransform_should_set_mocked_attribute_test() ->
  ?assertMatch(ok, util:refresh(mock_dummy, [{source_dir, "test/fixtures/"}, {parse_transform, eunit_mock}])),
  ?assertMatch({value,{mocked,[true]}}, lists:keysearch(mocked, 1, mock_dummy:module_info(attributes))).

test_module_should_not_have_mocked_attribute_test() ->
  ?assertMatch(ok, util:refresh(mock_dummy, [{source_dir, "test/fixtures/"}, {d, 'TEST'}])),
  ?assertMatch(false, lists:keysearch(mocked, 1, mock_dummy:module_info(attributes))).

  
dummy1_test() ->
  ok.