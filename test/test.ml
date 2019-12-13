let suite = Bedrock_test.suite @ Paperwork_test.suite @ Shapes_test.suite
let () = Alcotest.run "planet" suite
