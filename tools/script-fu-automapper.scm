#!/usr/bin/env gimp-script-fu-interpreter-3.0

;; NOTES
;; SF-TOGGLE actually returns 1 or 0 and not #t or #f
;; Script will quit if (in order checked): user not ready, no images open, more than one image open, unsaved changes, image dimensions wrong, layer count wrong

(script-fu-register
    "script-fu-automapper"                       ; function name
    "Automapper"                                 ; menu label
    "Urth Cartography map-making tool."          ; description
    "turtle"                                     ; author
    "Copyright 2026, urthmaps.com (MIT License)" ; copyright notice
    "September 7, 2026"                          ; date created
    ""                                           ; image type that the script works on
    SF-OPTION "OS" '("Linux" "MacOS" "Windows")  ; dropdown, which OS is being used
    SF-DIRNAME "Local repository root" ""        ; working directory, should be urthmaps repo root
    SF-TOGGLE "Enable debugging?" #t             ; checkbox, enables debug output
    SF-TOGGLE "Ready to start?" #f               ; checkbox, confirms that user is ready
)

;; Register the script so that it appears under Image > Urth in the menus
(script-fu-menu-register "script-fu-automapper" "<Image>/Image/Urth")

;; Define map metainfo (number of layers, image height, and image width)
;; These are used to validate that the targeted map is the intended one
;; NOTE The layer count is based on the root of the file, meaning a folder counts as a layer, but any children do not (e.g. "Labels" counts as one).
(define expected_layer_count 6)
(define expected_image_height 7525)
(define expected_image_width 11232)

;; Main script function that is called by the interactive prompt (receives values from user input)
;; Order of variables depends on order in script-fu-register function
(define (script-fu-automapper option_os dir_working toggle_debug? toggle_ready?)

    (msg 1 "Starting script...")

    ; DEBUG: Enables output to error console if checked
    (cond ((= 1 toggle_debug?) ; Toggle debug enabled
            (msg 0 "Debug messages enabled.") ; Confirm state
            (msg 0 (string-append "Selected path is " dir_working)) ; Output path
            
            ; OS selection
            (cond ((= 0 option_os)
                   (msg 0 "Linux selected."))
                  ((= 1 option_os) 
                   (msg 0 "MacOS selected."))
                  ((= 2 option_os)
                   (msg 0 "Windows selected.")))
            
            ; User ready checkbox
            (cond ((= 1 toggle_ready?) 
                   (msg 0 "User ready."))
                  ((= 0 toggle_ready?) 
                   (msg 0 "User not ready."))))        

          ((= 0 toggle_debug?) ; Toggle debug disabled
           (msg 0 "Debug messages disabled.")))

    ; Terminate the script if the user is not ready or if more than one file is open
    (cond ((= 0 toggle_ready?) ; 
           (msg 3 "Please check \"ready\" before running. Exiting...")
           (quit))
          ((= 0 (vector-length (car (gimp-get-images)))) ; nothing open
           (msg 3 "You should have the political map open when running this script. Exiting...")
           (quit))
          ((> (vector-length (car (gimp-get-images))) 1) ; multiple files open
           (msg 3 "Close any other open images before running this script. Exiting...")
           (quit))
          ((= 1 toggle_debug?) (msg 0 "User is ready and image count check passed. Continuing...")))

    ; Get the image ID of urth.xcf (map_urth)
    (define map_urth (vector-ref (car (gimp-get-images)) 0))

    ; Terminate the script if the file does not meet expectations (layer count, dimensions, dirty)
    ; These are basic checks that the file is the intended one (urth.xcf)
    ; NOTE The metainfo values are defined near the top of this file.

    (cond ((= 1 (car (gimp-image-is-dirty map_urth))) ; dirty check
           (msg 3 "This image has unsaved changes. Exiting...")
           (quit))
          ((not (and ; dimension check
          (= expected_image_height (car (gimp-image-get-height map_urth)))
          (= expected_image_width (car (gimp-image-get-width map_urth)))))
           (msg 3 "This image does not match the expected size. Exiting...")
           (quit))
          ((not (= expected_layer_count (vector-length (car (gimp-image-get-layers map_urth))))) ; layer count check
           (msg 3 "This image does not have the expected number of layers. Please clean up temporary layers before starting this script. Exiting...")
           (quit))
          (else ; passed checks
           (msg 1 "Image seems correct. Proceeding...")))


    ; MAP - WORKING (temp map from which others are made)
    (msg 1 "Preparing base and borders for supplemental maps...")

    (define map_working (car (gimp-image-duplicate map_urth)))
    (cond ((= 1 toggle_debug?) (define display_working (car (gimp-display-new map_working)))))
    (deleter-layer map_working)

    ; Get the ID of the Political layer
    (define map_working_layer_political (car (gimp-image-get-layer-by-name map_working "Political")))

    ; Create the Base layer (white)
    (creator-layer map_working map_working_layer_political "Base" 1 1)

    ; Create the Borders layer (black)
    (creator-layer map_working map_working_layer_political "Borders" 0 4)

    ; Delete the political layer (no longer needed)
    (gimp-image-remove-layer map_working map_working_layer_political)

    ; Get the IDs of the layers
    (define map_working_layer_base (car (gimp-image-get-layer-by-name map_working "Base"))) ; find base
    (define map_working_layer_borders (car (gimp-image-get-layer-by-name map_working "Borders"))) ; find borders
    (define map_working_layer_ocean (car (gimp-image-get-layer-by-name map_working "Ocean"))) ; find ocean

    ; Remove locks from ocean (other locks handled in creator-layer function)
    (manager-lock map_working_layer_ocean 0)

    (msg 1 "Layers ready.")

    ; MAP - BLANK
    (msg 1 "Starting work on blank map...")

    (define map_blank (car (gimp-image-duplicate map_working))) ; duplicate working
    (cond ((= 1 toggle_debug?) (define display_blank (car (gimp-display-new map_blank))))) ; display blank if debug

    (define map_blank_layer_borders (car (gimp-image-get-layer-by-name map_blank "Borders"))) ; find borders layer ID
    (gimp-image-merge-down map_blank map_blank_layer_borders 0) ; merge borders into base

    (saver-xcf option_os map_blank dir_working "blank" toggle_debug?)
    (saver-png option_os map_blank dir_working "blank" toggle_debug?)

    (cond ((= 1 toggle_debug?) ; if debug, delete display
           (gimp-display-delete display_blank))
          ((= 0 toggle_debug?) ; if not, delete image
           (gimp-image-delete map_blank)))

    (msg 1 "Blank map generated.")

    ; MAP - LOCATOR-GLOBAL
    (msg 1 "Starting work on global locator map...")

    (define map_lglobal (car (gimp-image-duplicate map_working))) ; duplicate working
    (cond ((= 1 toggle_debug?) (define display_lglobal (car (gimp-display-new map_lglobal))))) ; display lglobal if debug
    
    (define map_lglobal_layer_borders (car (gimp-image-get-layer-by-name map_lglobal "Borders"))) ; find borders
    (define map_lglobal_layer_base (car (gimp-image-get-layer-by-name map_lglobal "Base"))) ; find base
    (define map_lglobal_layer_ocean (car (gimp-image-get-layer-by-name map_lglobal "Ocean"))) ; find ocean

    (replacer-color map_lglobal map_lglobal_layer_base "white" '(185 185 185)) ; make base grey
    (replacer-color map_lglobal map_lglobal_layer_borders "black" "white") ; make borders white

    (gimp-image-merge-down map_lglobal map_lglobal_layer_borders 0) ; merge borders into base
    
    (gimp-drawable-fill map_lglobal_layer_ocean 3) ; fill ocean with white

    (saver-xcf option_os map_lglobal dir_working "locator-global" toggle_debug?)
    (saver-png option_os map_lglobal dir_working "locator-global" toggle_debug?)

    (cond ((= 1 toggle_debug?) ; if debug, delete display
           (gimp-display-delete display_lglobal))
          ((= 0 toggle_debug?) ; if not, delete image
           (gimp-image-delete map_lglobal)))

    (msg 1 "Global locator map generated.")

    ; MAP - LOCATOR-LOCAL
    (msg 1 "Starting work on local locator map...")

    (define map_llocal (car (gimp-image-duplicate map_working))) ; duplicate working
    (cond ((= 1 toggle_debug?) (define display_llocal (car (gimp-display-new map_llocal))))) ; display llocal if debug
    
    (define map_llocal_layer_borders (car (gimp-image-get-layer-by-name map_llocal "Borders"))) ; find borders
    (define map_llocal_layer_base (car (gimp-image-get-layer-by-name map_llocal "Base"))) ; find base
    (define map_llocal_layer_ocean (car (gimp-image-get-layer-by-name map_llocal "Ocean"))) ; find ocean

    (creator-outline map_llocal map_llocal_layer_base "white" 5 '(70 112 177) 0) ; create navy blue outline around base
    (replacer-color map_llocal map_llocal_layer_base "white" '(252 254 231)) ; make base beige

    (creator-outline map_llocal map_llocal_layer_borders "black" 2 '(100 102 102) 0) ; widen borders (by 2, final width of 5px) 
    (replacer-color map_llocal map_llocal_layer_borders "black" '(100 102 102)) ; make borders grey

    (gimp-image-merge-down map_llocal map_llocal_layer_borders 0) ; merge borders into base

    (gimp-context-set-background '(210 232 255)) ; set background to light blue
    (gimp-drawable-fill map_llocal_layer_ocean 1) ; fill ocean with background

    (saver-xcf option_os map_llocal dir_working "locator-local" toggle_debug?)
    (saver-png option_os map_llocal dir_working "locator-local" toggle_debug?)

    (cond ((= 1 toggle_debug?) ; if debug, delete display
           (gimp-display-delete display_llocal))
          ((= 0 toggle_debug?) ; if not, delete image
           (gimp-image-delete map_llocal)))

    (msg 1 "Local locator map generated.")

    ; MAP - HYDRO
    (msg 1 "Starting work on hydro map...")

    (define map_hydro (car (loader-xcf option_os dir_working "hydro" toggle_debug?))) ; load the existing hydro map
    (cond ((= 1 toggle_debug?) (define display_hydro (car (gimp-display-new map_hydro))))) ; display hydro if debug

    (define map_hydro_layer_base (car (gimp-image-get-layer-by-name map_hydro "Base"))) ; find base

    (gimp-selection-all map_working); select all on working
    (gimp-edit-named-copy (vector map_working_layer_base) "buffer_base"); copy to buffer_base (function only accepts vectors)

    (gimp-item-set-lock-content map_hydro_layer_base 0) ; unlock base content
    (gimp-layer-set-lock-alpha map_hydro_layer_base 0) ; unlock base alpha
    (gimp-selection-all map_hydro) ; select all on hydro 
    (gimp-drawable-edit-fill map_hydro_layer_base 4) ; clear the layer (fill transparency)
    (define map_hydro_floating_base (car (gimp-edit-named-paste map_hydro_layer_base "buffer_base" 1))) ; paste from buffer_base and keep floating ID
    (gimp-floating-sel-anchor map_hydro_floating_base) ; merge down floating
    (gimp-item-set-lock-content map_hydro_layer_base 1) ; lock base content
    (gimp-layer-set-lock-alpha map_hydro_layer_base 1) ; lock base alpha

    (saver-xcf option_os map_hydro dir_working "hydro" toggle_debug?)
    (saver-png option_os map_hydro dir_working "hydro" toggle_debug?)

    (cond ((= 1 toggle_debug?) ; if debug, delete display
           (gimp-display-delete display_hydro))
          ((= 0 toggle_debug?) ; if not, delete image
           (gimp-image-delete map_hydro)))

    (gimp-buffer-delete "buffer_base") ; delete buffer

    (msg 1 "Hydro map base replaced.")

    ; MAP - INTERACTIVE
    (msg 1 "Starting work on interactive map...")

    (define map_interactive (car (gimp-image-duplicate map_working))) ; duplicate working
    (cond ((= 1 toggle_debug?) (define display_interactive (car (gimp-display-new map_interactive))))) ; display interactive if debug

    (define map_interactive_layer_borders (car (gimp-image-get-layer-by-name map_interactive "Borders"))) ; find borders
    (define map_interactive_layer_base (car (gimp-image-get-layer-by-name map_interactive "Base"))) ; find base
    (define map_interactive_layer_ocean (car (gimp-image-get-layer-by-name map_interactive "Ocean"))) ; find ocean
    (define map_urth_layer_political (car (gimp-image-get-layer-by-name map_urth "Political"))) ; find political in urth

    (gimp-image-undo-disable map_urth) ; disable undo tracking on urth
    (gimp-selection-all map_urth) ; select all on urth
    (gimp-edit-named-copy (vector map_urth_layer_political) "buffer_political") ; copy to buffer_political (function only accepts vectors)
    (gimp-selection-none map_urth) ; deselect all on urth
    (gimp-image-undo-enable map_urth) ; reenable undo tracking on urth
    (gimp-image-clean-all map_urth) ; remove the dirty state from urth, since no changes were actually made

    (gimp-selection-all map_interactive) ; select all on interactive
    (gimp-drawable-edit-fill map_interactive_layer_base 4) ; clear the layer (fill transparency)
    (define map_interactive_floating_political (car (gimp-edit-named-paste map_interactive_layer_base "buffer_political" 1))) ; paste from buffer_base and keep floating ID 
    (gimp-floating-sel-anchor map_interactive_floating_political) ; merge down floating
    (gimp-selection-none map_interactive) ; deselect

    (replacer-color map_interactive map_interactive_layer_base '(66 86 146) '(224 227 237)) ; blue
    (replacer-color map_interactive map_interactive_layer_base '(41 137 49) '(220 236 221)) ; green
    (replacer-color map_interactive map_interactive_layer_base '(127 127 127) '(234 234 234)) ; grey
    (replacer-color map_interactive map_interactive_layer_base '(118 97 133) '(232 229 235)) ; purple
    (replacer-color map_interactive map_interactive_layer_base '(185 63 49) '(243 224 221)) ; red
    (replacer-color map_interactive map_interactive_layer_base '(217 171 33) '(248 241 219)) ; yellow

    (creator-outline map_interactive map_interactive_layer_borders "black" 3 '(159 160 160) 1) ; widen borders (by 3, final width of 7px)

    (gimp-image-merge-down map_interactive map_interactive_layer_borders 0) ; merge borders into base

    (gimp-context-set-background '(176 197 213)) ; set background to light blue
    (gimp-drawable-fill map_interactive_layer_ocean 1) ; fill ocean with background

    (msg 1 "Downscaling interactive map, this may take a few moments...")
    (gimp-image-scale map_interactive 4269 2860) ; resize image before saving

    (saver-xcf option_os map_interactive dir_working "interactive" toggle_debug?)
    (saver-png option_os map_interactive dir_working "interactive" toggle_debug?)

    (cond ((= 1 toggle_debug?) ; if debug, delete display
           (gimp-display-delete display_interactive))
          ((= 0 toggle_debug?) ; if not, delete image
           (gimp-image-delete map_interactive)))

    (gimp-buffer-delete "buffer_political") ; delete buffer

    (msg 1 "Interactive map generated.")

    ; MAP - TIMEZONES
    (msg 1 "Starting work on timezones map...")

    (define map_timezones (car (loader-xcf option_os dir_working "timezones" toggle_debug?))) ; load the existing timezones map
    (cond ((= 1 toggle_debug?) (define display_timezones (car (gimp-display-new map_timezones))))) ; display timezones if debug

    (define map_timezones_layer_base (car (gimp-image-get-layer-by-name map_timezones "Base"))) ; find base

    (gimp-image-select-color map_timezones 0 map_timezones_layer_base '(125 189 209)) ; select blue
    (gimp-image-select-color map_timezones 0 map_timezones_layer_base '(167 214 150)) ; select green
    (gimp-image-select-color map_timezones 0 map_timezones_layer_base '(225 171 64)) ; select orange
    (gimp-image-select-color map_timezones 0 map_timezones_layer_base '(170 138 193)) ; select purple
    (gimp-image-select-color map_timezones 0 map_timezones_layer_base '(238 99 98)) ; select red
    (gimp-image-select-color map_timezones 0 map_timezones_layer_base '(254 224 144)) ; select yellow

    (gimp-edit-named-copy (vector map_timezones_layer_base) "buffer_timezones_colors") ; copy colours to buffer
    
    (gimp-selection-all map_working) ; select all in working
    (gimp-edit-named-copy (vector map_working_layer_base) "buffer_working_base") ; copy base from working to buffer
    (gimp-edit-named-copy (vector map_working_layer_borders) "buffer_working_borders") ; copy borders from working to buffer
    (gimp-selection-none map_working) ; deselect

    (gimp-selection-all map_timezones) ; select all in timezones
    (gimp-drawable-edit-fill map_timezones_layer_base 4) ; clear the layer (fill transparency)
    (define map_timezones_floating_base (car (gimp-edit-named-paste map_timezones_layer_base "buffer_working_base" 1))) ; paste base from working
    (gimp-buffer-delete "buffer_working_base") ; delete buffer
    (gimp-floating-sel-anchor map_timezones_floating_base) ; merge down floating
    (gimp-selection-none map_timezones) ; deselect
    (gimp-item-transform-translate map_timezones_layer_base 1 0) ; shift base over by one pixel so it aligns properly
    (gimp-layer-resize-to-image-size map_timezones_layer_base) ; trim off any excess pixels

    (mover-area map_timezones map_timezones_layer_base 314 0 314 7525 11232 0) ; move starting slice to end
    (define map_timezones_layer_base (car (gimp-image-get-layer-by-name map_timezones "Base"))) ; find base (again, ID changes when merged down)
    (mover-area map_timezones map_timezones_layer_base 11232 0 315 7525 -11232 0) ; move ending slice to start
    (define map_timezones_layer_base (car (gimp-image-get-layer-by-name map_timezones "Base"))) ; find base (again, ID changes when merged down)

    (define map_timezones_floating_borders (car (gimp-edit-named-paste map_timezones_layer_base "buffer_working_borders" 1))) ; paste borders from working
    (gimp-buffer-delete "buffer_working_borders") ; delete buffer
    (gimp-floating-sel-to-layer map_timezones_floating_borders) ; create new layer with floating (keeps same ID)
    
    (replacer-color map_timezones map_timezones_layer_base "white" '(198 198 198)) ; recolour base grey
    (replacer-color map_timezones map_timezones_floating_borders "black" "white") ; recolour borders white
    
    (define map_timezones_layer_base (car (gimp-image-get-layer-by-name map_timezones "Base"))) ; find base (in case ID changes when merged down)

    (cond ((= (cadar (gimp-image-pick-color map_timezones (vector map_timezones_floating_borders) 9609 4793 1 0 0)) 198) ; if the border of Blueacia is grey (only checking R value)
           (gimp-item-transform-translate map_timezones_floating_borders 1 0))) ; move one px right

    (define map_timezones_floating_colors (car (gimp-edit-named-paste map_timezones_layer_base "buffer_timezones_colors" 1))) ; paste colours from buffer
    (gimp-item-transform-translate map_timezones_floating_colors 0 64) ; move layer down so it aligns properly
    (cond ((= (cadar (gimp-image-pick-color map_timezones (vector map_timezones_floating_colors) 0 7524 1 0 0)) 198) ; if the bottom left corner is grey (only checking R value)
           (cond ((= (cadar (gimp-image-pick-color map_timezones (vector map_timezones_floating_colors) 1 7524 1 0 0)) 198) ; 2px off
                  (gimp-item-transform-translate map_timezones_floating_colors -2 0))
                 (else (gimp-item-transform-translate map_timezones_floating_colors -1 0)))))
    (gimp-floating-sel-anchor map_timezones_floating_colors) ; anchor floating

    (gimp-image-merge-down map_timezones map_timezones_floating_borders 0) ; merge borders into base

    (saver-xcf option_os map_timezones dir_working "timezones" toggle_debug?)
    (saver-png option_os map_timezones dir_working "timezones" toggle_debug?)

    (cond ((= 1 toggle_debug?) ; if debug, delete display
           (gimp-display-delete display_timezones))
          ((= 0 toggle_debug?) ; if not, delete image
           (gimp-image-delete map_timezones)))

    (gimp-buffer-delete "buffer_timezones_colors") ; delete buffer

    (msg 1 "Timezone map base and borders replaced.")

    (msg 1 "Process completed, cleaning up...")

    ; Automapper complete, removing working map
    (cond ((= 1 toggle_debug?) ; if debug, delete display
           (gimp-display-delete display_working))
          ((= 0 toggle_debug?) ; if not, delete image
           (gimp-image-delete map_working)))
    
    (msg 1 "End of script. Supplemental maps have been updated.")
)

;; Simple function that appends stuff before a message and sends it to the error console.
;; Accepted types: debug (0), message (1), warning (2), error (3)
(define (msg type input)

    (cond ((= 0 type)
           (gimp-message (string-append "DEBUG: " input)))
          ((= 1 type)
           (gimp-message (string-append "MESSAGE: " input)))
          ((= 2 type)
           (gimp-message (string-append "WARNING: " input)))
          ((= 3 type)
           (gimp-message (string-append "ERROR: " input)))
          (else (gimp-message "Not a recognized message type!")))

)

;; Function to select map colours with additional switches for black and white
;; Use 1 (true) or 0 (false) for the black/white selection
(define (selector-color image drawable select_color? select_white? select_black?)
    
    ; Select regular colours, as requested
    ; NOTE 0 adds to a selection
    (cond ((= 1 select_color?)
           (gimp-image-select-color image 0 drawable '(66 86 146)) ; blue
           (gimp-image-select-color image 0 drawable '(41 137 49)) ; green
           (gimp-image-select-color image 0 drawable '(127 127 127)) ; grey
           (gimp-image-select-color image 0 drawable '(118 97 133)) ; purple
           (gimp-image-select-color image 0 drawable '(185 63 49)) ; red
           (gimp-image-select-color image 0 drawable '(217 171 33)))) ; yellow
    
    ; Select black and white, as requested
    (cond ((= 1 select_white?)
           (gimp-image-select-color image 0 drawable "white")))
    
    (cond ((= 1 select_black?)
           (gimp-image-select-color image 0 drawable "black")))

)

;; Function to delete unused layers
(define (deleter-layer image)
    
    ; Find layers by name and delete them
    (gimp-image-remove-layer image (car (gimp-image-get-layer-by-name image "Update Notes")))
    (gimp-image-remove-layer image (car (gimp-image-get-layer-by-name image "Legend")))
    (gimp-image-remove-layer image (car (gimp-image-get-layer-by-name image "Latitude/Longitude")))
    (gimp-image-remove-layer image (car (gimp-image-get-layer-by-name image "Labels")))

)

;; Function to save XCF files
;; /home/turtle/Projects/Urth/urthmaps/maps/source/blank.xcf
;; path_directory---------------------|-----------|path_file
(define (saver-xcf os image path_directory path_file enable_debug?)
    (cond ((or (= 0 os) (= 1 os)) ; Linux or MacOS
           (cond ((= 1 enable_debug?)
                  (msg 0 (string-append "Export path is " path_directory "/maps/source/" path_file ".xcf (Linux or MacOS)"))))
           (gimp-xcf-save 0 image (string-append path_directory "/maps/source/" path_file ".xcf")))
          ((= 2 os) ; Windows
           (cond ((= 1 enable_debug?)
                  (msg 0 (string-append "Export path is " path_directory "\\maps\\source\\" path_file ".xcf (Windows)"))))
           (gimp-xcf-save 0 image (string-append path_directory "\\maps\\source\\" path_file ".xcf"))))
)

;; Function to export PNG files
;; /home/turtle/Projects/Urth/urthmaps/maps/export/blank.png
;; path_directory---------------------|-----------|path_file
(define (saver-png os image path_directory path_file enable_debug?)
    (cond ((or (= 0 os) (= 1 os)) ; Linux or MacOS
           (cond ((= 1 enable_debug?)
                  (msg 0 (string-append "Export path is " path_directory "/maps/export/" path_file ".png (Linux or MacOS)"))))
           (file-png-export 1 image (string-append path_directory "/maps/export/" path_file ".png") 0 0 9 1 1 1 1 0 0 "auto" 0 0 0 0 0 0))
          ((= 2 os) ; Windows
           (cond ((= 1 enable_debug?)
                  (msg 0 (string-append "Export path is " path_directory "\\maps\\export\\" path_file ".png (Windows)"))))
           (file-png-export 1 image (string-append path_directory "\\maps\\export\\" path_file ".png") 0 0 9 1 1 1 1 0 0 "auto" 0 0 0 0 0 0)))
)

;; This function replaces a colour with another on a given layer
(define (replacer-color image drawable color_before color_after)
    (gimp-image-select-color image 0 drawable color_before) ; select the old colour
    (gimp-context-set-background color_after) ; set the new colour as background
    (gimp-drawable-edit-fill drawable 1) ; fill selection with background
    (gimp-selection-none image) ; deselect
)

;; This function creates the border and base layers
;; layer_type: border (0), base (1)
;; fill_type: foreground (0), background (1), transparent (4)
(define (creator-layer image layer_source layer_name layer_type fill_type)
    (define layer_new (car (gimp-layer-copy layer_source))) ; clone the selected layer
    (gimp-item-set-name layer_new layer_name) ; set the name of the layer
    (manager-lock layer_new 0) ; remove all the locks
    (gimp-image-insert-layer image layer_new 0 0) ; insert layer at the top of the stack
    (selector-color image layer_new 1 1 layer_type) ; select colours, white, and (depending on choice) black
    (gimp-context-set-default-colors) ; set active colours to default (fg black, bg white)
    (gimp-drawable-edit-fill layer_new fill_type) ; fill selection with selected fill type
    (gimp-selection-none image) ; deselect
)

;; This function draws an "outline" on the same layer using the selection-grow procedure. Also works to widen borders.
;; inner_color: color of the object to outline (will be excluded from selection)
;; outline_size: size of outline, in pixels
;; outline_color: color of the outline
;; change_inner?: whether or not to change the inner object's colour as well (1/0)
(define (creator-outline image drawable inner_color outline_size outline_color change_inner?)
    (gimp-image-select-color image 0 drawable inner_color) ; select object to outline
    (gimp-selection-grow image outline_size) ; grow selection
    ;(gimp-image-select-color image 1 drawable inner_color) ; remove object from selection (leaving only outline)
    (cond ((= 0 change_inner?) ; don't change the inner, remove it from the selection (leaving only the outline)
           (gimp-image-select-color image 1 drawable inner_color)))
    (gimp-context-set-background outline_color) ; set background color to desired outline color
    (gimp-drawable-edit-fill drawable 1) ; fill selection with background colour    
    (gimp-selection-none image) ; deselect
)

;; This function loads XCF files into GIMP
(define (loader-xcf os path_directory path_file enable_debug?)
    (cond ((or (= 0 os) (= 1 os)) ; Linux or MacOS
           (cond ((= 1 enable_debug?)
                  (msg 0 (string-append "Loaded file is " path_directory "/maps/source/" path_file ".xcf (Linux or MacOS)"))))
           (gimp-xcf-load 1 (string-append path_directory "/maps/source/" path_file ".xcf")))
          ((= 2 os) ; Windows
           (cond ((= 1 enable_debug?)
                  (msg 0 (string-append "Loaded file is " path_directory "\\maps\\source\\" path_file ".xcf (Windows)"))))
           (gimp-xcf-load 1 (string-append path_directory "\\maps\\source\\" path_file ".xcf"))))
)

;; This function copies an area and translates it elsewhere
;; coord_x/y: top-left corner of the initial selection
;; select_w/h: the dimensions of the selection rectangle
;; move_x/y: the amount to move in each direction once pasted
(define (mover-area image drawable coord_x coord_y select_width select_height move_x move_y)
    (gimp-image-select-rectangle image 0 coord_x coord_y select_width select_height) ; select an area, coords are for top left corner
    (gimp-edit-copy (vector drawable)) ; copy the area
    (define mover_floating (vector-ref (car (gimp-edit-paste drawable 1)) 0)) ; paste the area to the same layer (floating)
    (gimp-item-transform-translate mover_floating move_x move_y) ; move the floating layer as specified
    (gimp-floating-sel-to-layer mover_floating) ; turn floating selection into a temp layer 
    (gimp-image-merge-down image mover_floating 0) ; merge back onto source layer (ID stil the same for floating layer)
    (gimp-selection-none image) ; deselect
    ; NOTE the ID of the drawable used here needs to be updated after (in the main function) because it will have changed.
)

;; This function toggles all lock types for a particular layer.
(define (manager-lock drawable lock?)
    (gimp-item-set-lock-content drawable lock?)
    (gimp-item-set-lock-position drawable lock?)
    (gimp-item-set-lock-visibility drawable lock?)
    (gimp-layer-set-lock-alpha drawable lock?)
)
