echo 'main = print root' > run.hs
xclip -o | sed -e 's/:/=/' -e 's/\//`div`/' >> run.hs
runhaskell run.hs
