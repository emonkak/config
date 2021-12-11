export PATH
export MANPATH

for package in coreutils findutils gawk gnu-sed gnu-tar grep
do
  if [ -d "/usr/local/opt/$package/libexec/gnubin" ]
  then
    PATH="/usr/local/opt/$package/libexec/gnubin:$PATH"
  fi
  if [ -d "/usr/local/opt/$package/libexec/gnuman" ]
  then
    MANPATH="/usr/local/opt/$package/libexec/gnuman:$MANPATH"
  fi
done

PATH="$HOME/bin:$PATH"
