module Project = struct
  let boot _potential_textarea _potential_container =
    Console.print "boot !"
  ;;

  let api =
    object%js
      method boot textarea container = boot textarea container
    end
  ;;
end
