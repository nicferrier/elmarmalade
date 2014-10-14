;;; marmalade-vars.el --- some constants for marmalade

;;; Code:

(defconst marmalade/page-header "<div class=\"navbar\">
            <div class=\"navbar-inner\">
                <ul class=\"nav pull-right\">
                    <li class=\"active\"><a href=\"/\">marmalade-repo</a></li>
                    <li><a href=\"/login/\">login</a></li>
                    <li><a href=\"/register-comingsoon/\">register</a></li>
                </ul>
            </div>
        </div>
        <div id=\"github\">
            <a href=\"https://github.com/nicferrier/elmarmalade\">
                <img style=\"position: absolute; top: 0; left: 0; border: 0;\" 
                     src=\"https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png\" 
                     alt=\"Fork me on GitHub\"></img>
            </a>
        </div>"
  "The page header for logged out users.")

(defconst marmalade/page-header-loggedin "<div class=\"navbar\">
            <div class=\"navbar-inner\">
                <ul class=\"nav pull-right\">
                    <li class=\"active\"><a href=\"/\">marmalade-repo</a></li>
                    <li><a href=\"/profiles/${username}/\">${username}</a></li>
                </ul>
            </div>
        </div>
        <div id=\"github\">
            <a href=\"https://github.com/nicferrier/elmarmalade\">
                <img style=\"position: absolute; top: 0; left: 0; border: 0;\" 
                     src=\"https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png\" 
                     alt=\"Fork me on GitHub\"></img>
            </a>
        </div>"
  "The page header for logged-in users.")


(provide 'marmalade-vars)

;;; marmalade-vars.el ends here
