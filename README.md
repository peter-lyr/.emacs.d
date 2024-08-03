https://emacs.stackexchange.com/questions/33403/customize-creates-custom-set-faces-unintentionally
https://emacs.stackexchange.com/questions/59694/cant-get-rid-of-scratch-buffer-at-startup-for-emacs-26-3
https://emacs.stackexchange.com/questions/47771/should-i-use-require-or-load-when-writing-my-own-configuration
https://www.emacswiki.org/emacs/LineNumbers
https://www.orgroam.com/manual.html#Installation
https://emacs.stackexchange.com/questions/20167/how-do-i-create-a-borderless-frame
https://emacs.stackexchange.com/questions/22663/how-can-transparency-be-toggled
https://github.com/purcell/emacs.d/blob/master/init.el

if和when语句
https://blog.csdn.net/csfreebird/article/details/7477396

let语句
(let VARLIST BODY)
先赋值，在跑body，跑完body后，刚刚赋值的变量失效
val1,val2,...是一下整个括号内的局部变量
(let ((val1 VAL1)(val2 VAL2)...)
  (...))

Windows HONE
在cmd输入regedit后回车就会打开注册表了
找到HKEY_LOCAL_MACHINE\SOFTWARE\GNU\Emacs（如果没有则手动添加项），在此项下添加字符串值，名称为HOME， 值为D:\emacs.d。这样做的目的是让D:\emacs.d成为Emacs的home路径
有效
