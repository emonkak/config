BEGIN {
  print ";; okuri-ari entries."
  print ";; okuri-nasi entries."
}

/^<pre>/ {
  sub("<pre>","",$1)
  print $1 " /" $2 "/"
}

/^[^<]/ {
  print $1 " /" $2 "/"
}
