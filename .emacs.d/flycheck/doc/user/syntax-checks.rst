.. _flycheck-syntax-checks:

===============
 Check buffers
===============

Flycheck provides two Emacs minor modes for automatic syntax checking:
:mode:`flycheck` to enable syntax checking in the current buffer, and
:mode:`global-flycheck` to enable syntax checking in all buffers whenever
possible.

.. minor-mode:: flycheck-mode

   Enable :ref:`automatic syntax checking <flycheck-automatic-checks>` in the
   current buffer.

.. minor-mode:: global-flycheck-mode

   Enable :mode:`flycheck` in all buffers where syntax checking is possible.

   .. note::

      This mode does not enable :mode:`flycheck` in remote files (via
      TRAMP) and encrypted files.  Checking remote files may be very slow
      depending on the network connections, and checking encrypted files would
      leak confidential data to temporary files and subprocesses.

      You can manually enable :mode:`flycheck` in these buffers nonetheless, but
      we do *not* recommend this for said reasons.

   Add the following to your :term:`init file` to enable syntax checking
   permanently:

   .. code-block:: elisp

      (add-hook 'after-init-hook #'global-flycheck-mode)

   You can exclude specific major modes from syntax checking with
   `flycheck-global-modes`:

   .. defcustom:: flycheck-global-modes

      Major modes for which :mode:`global-flycheck` turns on :mode:`flycheck`:

      ``t`` (the default)
         Turn :mode:`flycheck` on for all major modes.

      :samp:`({foo-mode} …)`
         Turn :mode:`flycheck` on for all major modes in this list,
         i.e. whenever the value of ``major-mode`` is contained in this list.

      :samp:`(not {foo-mode} …)`
         Turn :mode:`flycheck` on for all major nodes *not* in this list,
         i.e. whenever the value of ``major-mode`` is *not* contained in this
         list.

      .. note::

         :mode:`global-flycheck` never turns on :mode:`flycheck` in major modes
         whose ``mode-class`` property is ``special``, regardless of the value
         of this option.  Syntax checking simply makes no sense in special
         buffers which are typically intended for non-interactive display rather
         than editing.

      .. seealso::

         :infonode:`(elisp)Major Mode Conventions`
            Information about major modes, and modes marked as special.

.. _flycheck-automatic-checks:

Check automatically
===================

By default :mode:`flycheck` automatically checks a buffer whenever

* it is enabled,
* the buffer is saved,
* a new line is inserted,
* or a short time after the last change was made in a buffer.

You can customise this behaviour with `flycheck-check-syntax-automatically`:

.. defcustom:: flycheck-check-syntax-automatically

   A list of events which trigger a syntax check in the current buffer:

   ``save``
      Check the buffer immediately after it was saved.

   ``new-line``
      Check the buffer immediately after a new line was inserted.

   ``idle-change``
      Check the buffer a short time after the last change.  The delay is
      customisable with `flycheck-idle-change-delay`:

      .. defcustom:: flycheck-idle-change-delay

         Seconds to wait after the last change to the buffer before starting a
         syntax check.

   ``mode-enabled``
      Check the buffer immediately after :mode:`flycheck` was enabled.

   For instance with the following setting :mode:`flycheck` will only check the
   buffer when it was saved:

   .. code-block:: elisp

      (setq flycheck-check-syntax-automatically '(mode-enabled save))

.. _flycheck-manual-checks:

Check manually
==============

You can also start a syntax check explicitly with `C-c ! c`:

.. define-key:: C-c ! c
                M-x flycheck-buffer

   Check syntax in the current buffer.

Debug syntax checking
=====================

To make sure that syntax checking works correctly verify your setup:

.. define-key:: C-c ! v
                M-x flycheck-verify-setup

   Show a buffer with information about your :mode:`flycheck` setup for the
   current buffer.

   Lists all syntax checkers available for the current buffer, and potential
   issues with their setup.
