+++
title = "{{ replace .Name "-" " " | title }}"
tags = ["good"]

[[resources]]
    name = "photo"
    title = "{{ replace .Name "-" " " | title }}"
    src = "*.jpg"
    [resources.params]
        alt = "{{ replace .Name "-" " " | title }}"
+++