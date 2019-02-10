include Bedrock.Uuid.Generator (struct
  let name = Unix.gethostname
  let pid = Unix.getpid
  let time = Unix.gettimeofday
end)
