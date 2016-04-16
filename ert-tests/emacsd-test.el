;;; emacsd-test.el --- Tests for .emacs.d

;; Dumb tests to check if config files exists
(ert-deftest emacsd-init-file-exists-test ()
  "Check if init.el exists"
  (should (equal (file-exists-p "../init.el") t)))

(ert-deftest emacsd-general_config-file-exists-test ()
  "Check if general_config.el exists"
  (should (equal (file-exists-p "../general_config.el") t)))

(ert-deftest emacsd-install-file-exists-test ()
  "Check if install.el exists"
  (should (equal (file-exists-p "../install.el") t)))

(ert-deftest emacsd-defalias-file-exists-test ()
  "Check if defalias.el exists"
  (should (equal (file-exists-p "../defalias.el") t)))

(ert-deftest emacsd-kbd_macro-file-exists-test ()
  "Check if kbd_macro.el exists"
  (should (equal (file-exists-p "../kbd_macro.el") t)))

(ert-deftest emacsd-package_init-file-exists-test ()
  "Check if kbd_package_init.el exists"
  (should (equal (file-exists-p "../package_init.el") t)))

;; Optional
(ert-deftest emacsd-gnus-file-exists-test ()
  "Check if gnus.el exists"
  (should (equal (file-exists-p "../gnus.el") t)))

;; TODO: Test(s) for extensions/hidepw
