>{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
>{-# LANGUAGE ScopedTypeVariables, PackageImports #-}
>module GUI.XProto where
>import qualified Data.Map as Map
>import "byte-order" System.ByteOrder
>import Data.Binary
>import Data.Binary.Get
>import Data.ByteString.Lazy hiding (length)
>import Data.Int
>import Data.Word
>import Data.Bits
>-- ^ <https://www.x.org/releases/X11R7.5/doc/x11proto/proto.pdf>
>-- X Window System and X protocol are (C) 1986...2004, The Open Group.

>type BITMASK = Int32
>type Window = Int32
>type Pixmap = Int32
>type Cursor = Int32
>type Font = Int32
>type GContext = Int32
>type Colormap = Int32
>data Drawable = DrawableWindow Window | DrawablePixmap Pixmap
>  deriving (Eq,Show)
>data Fontable = FontableFont Font | FontableGContext GContext
>  deriving (Eq,Show)
>type Atom = Int32
>type VisualID = Int32
>type Value = Int32
>type Byte = Int8
>type INT8 = Int8
>type INT16 = Int16
>type INT32 = Int32
>type CARD8 = Word8
>type CARD16 = Word16
>type CARD32 = Word32
>type OpCode = Int8
>type Timestamp = CARD32
>type SetOfPointerEvent = SetOfEvent
>type SetOfDeviceEvent  = SetOfEvent
>type SetOfKeyMask = SetOfKeyButMask
>newtype SetOfKeyButMask = SetOfKeyButMask Word32
>  deriving (Eq, Show)
>  deriving newtype (Bits)
>shiftKeyButMask = SetOfKeyButMask 0x1
>lockKeyButMask = SetOfKeyButMask 0x2
>controlKeyButMask = SetOfKeyButMask 0x4
>mod1KeyButMask = SetOfKeyButMask 0x8
>mod2KeyButMask = SetOfKeyButMask 0x10
>mod3KeyButMask = SetOfKeyButMask 0x20
>mod4KeyButMask = SetOfKeyButMask 0x40
>mod5KeyButMask = SetOfKeyButMask 0x80
>button1KeyButMask = SetOfKeyButMask 0x100
>button2KeyButMask = SetOfKeyButMask 0x200
>button3KeyButMask = SetOfKeyButMask 0x400
>button4KeyButMask = SetOfKeyButMask 0x800
>button5KeyButMask = SetOfKeyButMask 0x1000
>newtype SetOfEvent = SetOfEvent Word32
>   deriving (Eq, Show, Binary)
>   deriving newtype (Bits)

>keyPressBit = SetOfEvent 0x1
>keyReleaseBit = SetOfEvent 0x2
>buttonPressBit = SetOfEvent 0x4
>buttonReleaseBit = SetOfEvent 0x8
>enterWindowBit = SetOfEvent 0x10
>leaveWindowBit = SetOfEvent 0x20
>pointerMotionBit = SetOfEvent 0x40
>pointerMotionHintBit = SetOfEvent 0x80
>button1MotionBit = SetOfEvent 0x100
>button2MotionBit = SetOfEvent 0x200
>button3MotionBit = SetOfEvent 0x400
>button4MotionBit = SetOfEvent 0x800
>button5MotionBit = SetOfEvent 0x1000
>buttonMotionBit = SetOfEvent 0x2000
>keymapStateBit = SetOfEvent 0x4000
>exposureBit = SetOfEvent 0x8000
>visibilityChangeBit = SetOfEvent 0x10000
>structureNotifyBit = SetOfEvent 0x20000
>resizeRedirectBit  = SetOfEvent 0x40000
>substructureNotifyBit = SetOfEvent 0x80000
>substructureRedirectBit = SetOfEvent 0x100000
>focusChangeBit = SetOfEvent 0x200000
>propertyChangeBit = SetOfEvent 0x400000
>colormapChangeBit =   SetOfEvent 0x800000
>ownerGrabButtonBit = SetOfEvent 0x1000000
>
>data BitGravity = BGForget|BGStatic | BGNorthWest | BGNorth | BGNorthEast
>                | BGWest | BGCenter | BGEast | BGSouthWest | BGSouth
>                | BGSouthEast deriving (Eq,Enum, Show)
>data WinGravity = WGUnmap | WGStatic | WGNorthWest | WGNorth | WGNorthEast
>                | WGWest | WGCenter | WGEast | WGSouthWest | WGSouth | WGSouthEast
>   deriving (Eq,Enum, Show)
>type BOOL = Bool
>data Event = KeyPress | KeyRelease | OwnerGrabButton | ButtonPress
>           | ButtonRelease | EnterWindow | LeaveWindow | PointerMotion
>           | PointerMotionHint | Button1Motion | Button2Motion
>           | Button3Motion | Button4Motion | Button5Motion | ButtonMotion
>           | Exposure | VisibilityChange | StructureNotify | ResizeRedirect
>           | SubstructureNotify | SubstructureRedirect | FocusChange
>           | PropertyChange | ColormapChange | KeymapState
>   deriving (Eq, Enum, Show)
>data PointerEvent = PEButtonPress | PEButtonRelease | PEEnterWindow
>  | PELeaveWindow | PEPointerMotion | PEPointerMotionHint | PEButton1Motion
>  | PEButton2Motion | PEButton3Motion | PEButton4Motion | PEButton5Motion
>  | PEButtonMotion | PEKeymapState
>  deriving (Eq,Enum, Show)
>type KeySym = Int32
>type KeyCode = CARD8
>type Button = CARD8
>data KeyMask = Shift | Lock | Control | Mod1 | Mod2 | Mod3 | Mod4 | Mod5
>  deriving (Eq,Enum, Show)
>data ButMask = BMButton1 | BMButton2 | BMButton3 | BMButton4 | BMButton5
>  deriving (Eq,Enum, Show)
>data KeyButMask = KeyButMaskKeyMask KeyMask | KeyButMaskButMask ButMask
>  deriving (Eq, Show)
>type STRING8 = [CARD8]
>type STRING16 = [CHAR2B]
>data CHAR2B = CHAR2B { char2b_byte1 :: CARD8, char2b_byte2 :: CARD8 }
>  deriving (Eq, Show)
>data Point = Point { point_x :: Int16, point_y :: Int16 }
>   deriving (Eq,Show)
>data Rectangle = Rectangle { rectangle_x :: Int16, rectangle_y :: Int16,
>                             rectangle_width :: CARD16, rectangle_height :: CARD16,
>                             rectangle_angle1 :: INT16, rectangle_angle2 :: INT16 }
>  deriving (Eq,Show)
>data HostFamily = Internet | InternetV6 | ServerInterpreted | DECnet | Chaos
>  deriving (Show)
>data Host = Host { host_family :: HostFamily,
>                   address :: [Byte] }
>  deriving (Show)
>data XError = XEAccess | XEAlloc | XEAtom | XEColormap | XECursor
>            | XEDrawable | XEFont | XEGCContext | XEIDChoice | XEImplementation
>            | XELength | XEMatch | XEName | XEPixmap | XERequest | XEValue
>            | XWindow
>   deriving (Eq,Show)
>data XSetupRequest = ConnectionSetup {
>   xconnectionsetup_protocol_major :: CARD16,
>   xconnectionsetup_protocol_minor :: CARD16,
>   xconnectionsetup_authorization_protocol_name :: STRING8,
>   xconnectionsetup_authorization_protocol_data :: STRING8 }
>  deriving (Show)
>getMany :: (Integral a, Binary b) => a -> Get [b]
>getMany 0 = return []
>getMany i = do { v <- get ; w <- getMany (i-1) ; return (v:w) }

>getBig :: (Binary a, Bytes a) => Get a
>getBig = do x <- get
>            return $ fromBigEndian x
>
>putBig :: (Binary a, Bytes a) => a -> Put
>putBig x = put $ toBigEndian x

>instance Binary XSetupRequest where
>   put (ConnectionSetup major minor pname pdata) = do
>     put (108 :: CARD8)
>     put (0 :: CARD8)
>     putBig major
>     putBig minor
>     putBig (fromIntegral (length pname) :: CARD16)
>     put pname
>     putBig (fromIntegral (length pdata) :: CARD16)
>     put pdata
>   get = do
>     (_ :: CARD16) <- get
>     ma <- get
>     mi <- get
>     (pnamelen :: CARD16) <- getBig
>     pname <- getMany pnamelen
>     (pdatalen :: CARD16) <- getBig
>     pdata <- getMany pdatalen
>     return $ ConnectionSetup ma mi pname pdata

>pad e = (4 - (e `mod` 4)) `mod` 4

>data XSetupResponseSuccess = XSRFailed {
>    protocol_major_version :: CARD16,
>    protocol_minor_version :: CARD16,
>    reason :: STRING8 }
> | XSRSuccess {
>     protocol_major_version :: CARD16,
>     protocol_minor_version :: CARD16,
>     vendor :: STRING8,
>     release_number :: CARD32,
>     resource_id_base :: CARD32,
>     resource_id_mask :: CARD32,
>     image_byte_order :: ImageByteOrder,
>     bitmap_scanline_unit :: ScanLineUnit,
>     bitmap_scanline_pad :: ScanLineUnit,
>     bitmap_bit_order :: BitmapBitOrder,
>     pixmap_formats :: [Format],
>     roots :: [Screen],
>     motion_buffer_size :: CARD32,
>     maximum_request_length :: CARD16,
>     min_keycode :: KeyCode,
>     max_keycode :: KeyCode
>     }
> | XSRAuthenticate {
>     reason :: STRING8
> }
>  deriving (Show)
>instance Binary XSetupResponseSuccess where
>   put _ = return ()
>   get = do
>     (su :: CARD8) <- get
>     case su of
>       0 -> do
>          (reasonlen :: CARD8) <- get
>          major <- getBig
>          minor <- getBig
>          (adatalen :: CARD16) <- getBig
>          reason <- getMany (fromIntegral reasonlen)
>          (unused :: [CARD8]) <- getMany (pad reasonlen)
>          return $ XSRFailed major minor reason
>       2 -> do
>          (unused :: [CARD8]) <- getMany 5
>          (adatalen :: CARD8) <- get
>          (reason :: [CARD8]) <- getMany (4*adatalen)
>          return $ XSRAuthenticate reason
>       1 -> do
>          (_ :: CARD8) <- get
>          major <- getBig
>          minor <- getBig
>          (adddatalen :: CARD16) <- getBig
>          releaseno <- getBig
>          rid_base <- getBig
>          rid_mask <- getBig
>          mbuffersize <- getBig
>          (vendorlen :: CARD16) <- getBig
>          maxreqlen <- getBig
>          (rootslen :: CARD8) <- get
>          (formatslen :: CARD8) <- get
>          byteorder <- get
>          bitorder <- get
>          scanlineunit <- get
>          scanlinepad <- get
>          minkeycode <- get
>          maxkeycode <- get
>          (_ :: CARD32) <- get
>          vendor <- getMany vendorlen
>          (_ :: [CARD8]) <- getMany (pad vendorlen)
>          formats <- getMany formatslen
>          roots <- getMany rootslen
>          return $ XSRSuccess major minor vendor releaseno rid_base
>                              rid_mask byteorder scanlineunit
>                              scanlinepad bitorder formats
>                              roots mbuffersize maxreqlen
>                              minkeycode maxkeycode

>data ImageByteOrder = LSBFirst | MSBFirst
>  deriving (Eq,Show)
>instance Binary ImageByteOrder where
>   get = do
>     (bo :: CARD8) <- get
>     if bo == 0 then return LSBFirst else return MSBFirst
>   put LSBFirst = put (0 :: CARD8)
>   put MSBFirst = put (1 :: CARD8)
>data ScanLineUnit = SLU8 | SLU16 | SLU32
>  deriving (Eq,Enum,Show)
>instance Binary ScanLineUnit where
>   put SLU8 = put (8 :: CARD8)
>   put SLU16 = put (16 :: CARD8)
>   put SLU32 = put (32 :: CARD8)
>   get = do
>    (slu :: CARD8) <- get
>    case slu of
>      8 -> return SLU8
>      16 -> return SLU16
>      32 -> return SLU32
>type BitsPerPixel = Int -- 1,4,8,16,24,32
>type ScanlinePad  = Int -- 8,16,32
>data BitmapBitOrder = LeastSignificant | MostSignificant
>  deriving (Eq,Show)
>instance Binary BitmapBitOrder where
>   put LeastSignificant = put (0 :: CARD8)
>   put MostSignificant = put (1 :: CARD8)
>   get = do
>     (bo :: CARD8) <- get
>     if bo == 0 then return LeastSignificant else return MostSignificant
>data Format = Format { format_depth :: CARD8,
>                       format_bits_per_pixel :: BitsPerPixel,
>                       format_scanline_pad :: ScanlinePad }
>  deriving (Show)
>instance Binary Format where
>   put (Format d bpp scan) = put d >> put bpp >> put scan
>   get = do
>     depth <- get
>     (bpp :: CARD8) <- get
>     (scanline :: CARD8) <- get
>     return $ Format depth (fromIntegral bpp) (fromIntegral scanline)
>data Screen = Screen {
>   screen_root :: Window,
>   screen_width_in_pixels :: CARD16,
>   screen_height_in_pixels :: CARD16,
>   screen_width_in_millimeters :: CARD16,
>   screen_height_in_millimeters :: CARD16,
>   screen_allowed_depths :: [Depth],
>   screen_root_depth :: CARD8,
>   screen_root_visual :: VisualID,
>   screen_default_colormap :: Colormap,
>   screen_white_pixel :: CARD32,
>   screen_black_pixel :: CARD32,
>   screen_min_installed_maps :: CARD16,
>   screen_max_installed_maps :: CARD16,
>   screen_backing_stores :: BackingStores,
>   screen_save_unders :: Bool,
>   screen_current_input_masks :: SetOfEvent }
>  deriving (Show)
>instance Binary Screen where
>   get = do
>     sr <- get
>     sdc <- get
>     swp <- getBig
>     sbp <- getBig
>     scim <- get
>     swip <- getBig
>     ship <- getBig
>     swimm <- getBig
>     shimm <- getBig
>     smim <- getBig
>     smam <- getBig
>     srv <- get
>     sbs <- get
>     ssu <- get
>     srd <- get
>     (sad_len :: CARD8) <- get
>     allowed_depths <- getMany sad_len
>     return $ Screen sr swip ship swimm shimm allowed_depths
>                     srd srv sdc swp sbp smim smam sbs ssu scim
>   put s = do
>     put (screen_root s)
>     put (screen_default_colormap s)
>     putBig (screen_white_pixel s)
>     putBig (screen_black_pixel s)
>     put (screen_current_input_masks s)
>     putBig (screen_width_in_pixels s)
>     putBig (screen_height_in_pixels s)
>     putBig (screen_width_in_millimeters s)
>     putBig (screen_height_in_millimeters s)
>     putBig (screen_min_installed_maps s)
>     putBig (screen_max_installed_maps s)
>     putBig (screen_root_visual s)
>     put (screen_backing_stores s)
>     put (screen_save_unders s)
>     put (screen_root_depth s)
>     put (fromIntegral (length (screen_allowed_depths s)) :: CARD8)
>     putMany (screen_allowed_depths s)
>data BackingStores = BSNever | BSWhenMapped | BSAlways
>  deriving (Eq,Show)
>instance Binary BackingStores where
>  put BSNever = put (0 :: CARD8)
>  put BSWhenMapped = put (1 :: CARD8)
>  put BSAlways = put (2 :: CARD8)
>  get = do
>    (u :: CARD8) <- get
>    case u of { 0 -> return BSNever; 1 -> return BSWhenMapped ; 2 -> return BSAlways }
>data VisualClass = VCStaticGray | VCStaticColor | VCTrueColor | VCGrayScale
>                 | VCPseudoColor | VCDirectColor  deriving (Eq,Enum, Show)
>instance Binary VisualClass where
>   put VCStaticGray = put (0 :: CARD8)
>   put VCGrayScale = put (1 :: CARD8)
>   put VCStaticColor = put (2 :: CARD8)
>   put VCPseudoColor = put (3 :: CARD8)
>   put VCTrueColor = put (4 :: CARD8)
>   put VCDirectColor = put (5 :: CARD8)
>   get = do
>     (u :: CARD8) <- get
>     case u of
>       0 -> return VCStaticGray
>       1 -> return VCGrayScale
>       2 -> return VCStaticColor
>       3 -> return VCPseudoColor
>       4 -> return VCTrueColor
>       5 -> return VCDirectColor
>       _ -> error $ "invalid VisualClass: " ++ show u
>data Depth = Depth {
>   depth_depth :: CARD8,
>   depth_visuals :: [VisualType] }
> deriving (Show)
>putMany [] = return ()
>putMany (c:cr) = put c >> putMany cr
>instance Binary Depth where
>  get = do
>    depth <- get
>    (_ :: CARD8) <- get
>    (visualslen :: CARD16) <- getBig
>    (_ :: CARD16) <- getBig
>    visuals <- getMany visualslen
>    return $ Depth depth visuals
>  put dep = do
>    put (depth_depth dep)
>    put (0 :: CARD8)
>    putBig ((fromIntegral $ length $ depth_visuals dep) :: CARD16)
>    putMany (depth_visuals dep)
>data VisualType = VisualType {
>  vt_visual_id :: VisualID,
>  vt_visual_class :: VisualClass,
>  vt_red_mask :: CARD32,
>  vt_green_mask :: CARD32,
>  vt_blue_mask :: CARD32,
>  vt_bits_per_rgb_value :: CARD8,
>  vt_colormap_entries :: CARD16 }
>  deriving (Show)
>instance Binary VisualType where
>   put vt = do
>     putBig (vt_visual_id vt)
>     put (vt_visual_class vt)
>     put (vt_bits_per_rgb_value vt)
>     putBig (vt_colormap_entries vt)
>     putBig (vt_red_mask vt)
>     putBig (vt_green_mask vt)
>     putBig (vt_blue_mask vt)
>     putBig (0 :: CARD32)
>   get = do
>     visualid <- getBig
>     visualclass <- get
>     bp_rgb_value <- get
>     cm_entries <- getBig
>     rmask <- getBig
>     gmask <- getBig
>     bmask <- getBig
>     (_ :: CARD32) <- get
>     return $ VisualType visualid visualclass rmask gmask bmask
>                         bp_rgb_value cm_entries
>     
>data XSetupResponse = ConnectionSetupResponseFailed {
>   csr_success :: XSetupResponseSuccess,
>   csr_protocol_major_version :: CARD16,
>   csr_protocol_minor_version :: CARD16,
>   csr_reason :: STRING8 }
>  | ConnectionSetupResponseAuthenticate {
>   csr_success :: XSetupResponseSuccess,
>   csr_reason :: STRING8 }
>  | ConnectionSetupResponseSuccess {
>   csr_success :: XSetupResponseSuccess,
>   csr_protocol_major_version :: CARD16,
>   csr_protocol_minor_version :: CARD16,
>   csr_vendor :: STRING8,
>   csr_release_number :: CARD32,
>   csr_resource_id_base :: CARD32,
>   csr_resource_id_mask :: CARD32,
>   csr_image_byte_order :: ImageByteOrder,
>   csr_bitmap_scanline_unit :: ScanLineUnit,
>   csr_bitmap_scanling_pad  :: ScanLineUnit,
>   csr_bitmap_bit_order :: BitmapBitOrder,
>   csr_pixmap_formats :: [Format],
>   csr_roots :: [Screen],
>   csr_motion_buffer_size :: CARD32,
>   csr_maximum_request_length :: CARD16,
>   csr_min_keycode :: KeyCode,
>   csr_max_keycode :: KeyCode }
>  deriving (Show)
>data XWABackgroundPixmap = XWABackgroundPixmap Pixmap | XWANone | XWAParentRelative
>  deriving (Eq,Show)
>data XWABackingStore = XWABSNotUseful | XWABSWhenMapped | XWABSAlways
>  deriving (Eq,Enum,Show)
>data XWAColormap = XWAColormapColormap Colormap | XWAColormapCopyFromParent
>  deriving (Eq,Show)
>data XWindowAttribute = XWAPixmap XWABackgroundPixmap
> | XWABackgroundPixel CARD32
> | XWABorderPixmap Pixmap
> | XWABorderPixel CARD32
> | XWABitGravity BitGravity
> | XWAWinGravity WinGravity
> | XWABackingStore XWABackingStore
> | XWABackingPlanes CARD32
> | XWABackingPixel CARD32
> | XWASaveUnder Bool
> | XWAEventMask (SetOfEvent)
> | XWADoNotPropagateMAsk (SetOfDeviceEvent)
> | XWAOverrideRedirect Bool
> | XWAColormap XWAColormap
> | XWACursor (Maybe Cursor)
>  deriving (Eq,Show)
>
>opcodetable_get :: Map.Map OpCode (Get XRequest)
>opcodetable_get = Map.fromList [
>  (1, createWindowGet),
>  (2, changeWindowAttributesGet),
>  (8, mapWindowGet)
> ]
>responsetable_get :: Map.Map OpCode (Get XRequestResponse)
>responsetable_get = Map.fromList [
>  (1, createWindowResponseGet),
>  (2, changeWindowAttributesResponseGet),
>  (8, mapWindowResponseGet)
> ]
>opcodetable_put :: Map.Map OpCode (XRequest -> Put)
>opcodetable_put = Map.fromList [
>  (1, createWindowPut),
>  (2, changeWindowPut),
>  (8, mapWindowPut)
> ]
>request_length z@(XCreateWindow{}) = 8 + length (xcreatewindow_value_list z)
>request_length z@(XChangeWindowAttributes{}) = 3 + length (xchangewindowattributes_value_list z)
>request_length z@(XMapWindow{}) = 8
>
>opcode :: XRequest -> OpCode
>opcode (XCreateWindow {}) = 1
>opcode (XChangeWindowAttributes {}) = 2
>opcode (XGetWindowAttributes {}) = 3
>opcode (XDestroyWindow {}) = 4
>opcode (XDestroySubwindows {}) = 5
>opcode (XChangeSaveSet {}) = 6
>opcode (XReparentWindow {}) = 7
>opcode (XMapWindow {}) = 8
>opcode (XMapSubwindows {}) = 9
>opcode (XUnmapWindow {}) = 10
>opcode (XUnmapSubwindows {}) = 11
>opcode (XConfigureWindow {}) = 12
>opcode (XCirculateWindow {}) = 13
>opcode (XGetGeometry {}) = 14
>opcode (XQueryTree {}) = 15
>opcode (XInternAtom {}) = 16
>opcode (XGetAtomName {}) = 17
>opcode (XChangeProperty {}) = 18
>opcode (XDeleteProperty {}) = 19
>opcode (XGetProperty {}) = 20
>opcode (XListProperties {}) = 21
>opcode (XSetSelectionOwner {}) = 22
>opcode (XGetSelectionOwner {}) = 23
>opcode (XConvertSelection {}) = 24
>opcode (XSendEvent {}) = 25
>opcode (XGrabPointer {}) = 26
>opcode (XUngrabPointer {}) = 27
>opcode (XGrabButton {}) = 28
>opcode (XUngrabButton {}) = 29
>opcode (XChangeActivePointerGrab {}) = 30
>opcode (XGrabKeyboard {}) = 31
>opcode (XUngrabKeyboard {}) = 32
>opcode (XGrabKey {}) = 33
>opcode (XUngrabKey {}) = 34
>opcode (XAllowEvents {}) = 35
>opcode (XGrabServer {}) = 36
>opcode (XUngrabServer {}) = 37
>opcode (XQueryPointer {}) = 38
>opcode (XGetMotionEvents{}) = 39
>opcode (XTranslateCoordinates {}) = 40
>opcode (XWarpPointer {}) = 41
>opcode (XSetInputFocus {}) = 42
>opcode (XGetInputFocus {}) = 43
>opcode (XQueryKeymap {}) = 44
>opcode (XOpenFont{}) = 45
>opcode (XCloseFont {}) = 46
>opcode (XQueryFont {}) = 47
>opcode (XQueryTextExtents {}) = 48
>opcode (XListFonts {}) = 49
>opcode (XListFontsWithInfo {}) = 50
>opcode (XSetFontPath {}) = 51
>opcode (XGetFontPath {}) = 52
>opcode (XCreatePixmap{}) = 53
>opcode (XFreePixmap{}) = 54
>opcode (XCreateGC{}) = 55
>opcode (XChangeGC{}) = 56
>opcode (XCopyGC{}) = 57
>opcode (XSetDashes{}) = 58
>opcode (XSetClipRectangles{}) = 59
>opcode (XFreeGC{}) = 60
>opcode (XClearArea{}) = 61
>opcode (XCopyArea{}) = 62
>opcode (XCopyPlane{}) = 63
>opcode (XPolyPoint {}) = 64
>opcode (XPolyLine {}) = 65
>opcode (XPolySegment{}) = 66
>opcode (XPolyRectangle{}) = 67
>opcode (XPolyArc{}) = 68
>opcode (XFillPoly{}) = 69
>opcode (XPolyFillRectangle{}) = 70
>opcode (XPolyFillArc{}) = 71
>opcode (XPutImage{}) = 72
>opcode (XGetImage{}) = 73
>opcode (XPolyText8{}) = 74
>opcode (XPolyText16{}) = 75
>opcode (XImageText8{}) = 76
>opcode (XImageText16{}) = 77
>opcode (XCreateColormap{}) = 78
>opcode (XFreeColormap{}) = 79
>opcode (XCopyColormapAndFree{}) = 80
>opcode (XInstallColormap{}) = 81
>opcode (XUninstallColormap{}) = 82
>opcode (XListInstalledColormaps{}) = 83
>opcode (XAllocColor{}) = 84
>opcode (XAllocNamedColor{}) = 85
>opcode (XAllocColorCells{}) = 86
>opcode (XAllocColorPlanes{}) = 87
>opcode (XFreeColors{}) = 88
>opcode (XStoreColors{}) = 89
>opcode (XStoreNamedColor{}) = 90
>opcode (XQueryColors{}) = 91
>opcode (XLookupColor{}) = 92
>opcode (XCreateCursor{}) = 93
>opcode (XCreateGlyphCursor{}) = 94
>opcode (XFreeCursor{}) = 95
>opcode (XRecolorCursor{}) = 96
>opcode (XQueryBestSize{}) = 97
>opcode (XQueryExtension{}) = 98
>opcode (XListExtensions{}) = 99
>opcode (XChangeKeyboardMapping{}) = 100
>opcode (XGetKeyboardMapping{}) = 101
>opcode (XChangeKeyboardControl{}) = 102
>opcode (XGetKeyboardControl{}) = 103
>opcode (XBell{}) = 104
>opcode (XChangePointerControl{}) = 105
>opcode (XGetPointerControl{}) = 106
>opcode (XSetScreenSaver{}) = 107
>opcode (XGetScreenSaver{}) = 108
>opcode (XChangeHosts{}) = 109
>opcode (XListHosts{}) = 110
>opcode (XSetAccessControl{}) = 111
>opcode (XSetCloseDownMode{}) = 112
>opcode (XKillClient{}) = 113
>opcode (XRotateProperties{}) = 114
>opcode (XForceScreenSaver{}) = 115
>opcode (XSetPointerMapping{}) = 116
>opcode (XGetPointerMapping{}) = 117
>opcode (XSetModifierMapping{}) = 118
>opcode (XGetModifierMapping{}) = 119
>opcode (XNoOperation{}) = 127

>decode_response :: OpCode -> ByteString -> XRequestResponse
>decode_response op inp = runGet (responsetable_get Map.! op) inp

>instance Binary XRequest where
>  get = do
>     (opcode :: Int8) <- get
>     (len :: Int16) <- getBig
>     opcodetable_get Map.! opcode
>  put z = do
>    let opc = opcode z
>    put opc
>    putBig (fromIntegral (request_length z) :: Int16)
>    (opcodetable_put Map.! opc) z

>changeWindowPut z = do
>    putBig (xr_wid z)
>    putBig (xchangewindowattributes_value_mask z)
>    put (xchangewindowattributes_value_list z)

>changeWindowAttributesGet = do
>    wid <- getBig
>    valuemask <- getBig
>    valuelist <- get
>    return $ XChangeWindowAttributes wid valuemask valuelist

>createWindowResponseGet :: Get XRequestResponse
>createWindowResponseGet = return XRRCreateWindow
>changeWindowAttributesResponseGet :: Get XRequestResponse
>changeWindowAttributesResponseGet = return XRRChangeWindowAttributes
>mapWindowResponseGet = return XRRMapWindow
>mapWindowGet = do
>    wid <- getBig
>    return $ XMapWindow wid

>createWindowGet = do
>    wid <- getBig
>    parent <- getBig
>    depth <- getBig
>    visual <- getBig
>    x <- getBig
>    y <- getBig
>    width <- getBig
>    height <- getBig
>    bw <- getBig
>    vm <- getBig
>    vl <- get
>    return $ XCreateWindow wid parent depth visual x y width height bw vm vl
>createWindowPut z = do
>    putBig (xr_wid z)
>    putBig (xr_parent z)
>    putBig (xcreatewindow_depth z)
>    putBig (xcreatewindow_visual z)
>    putBig (xcreatewindow_x z)
>    putBig (xcreatewindow_y z)
>    putBig (xcreatewindow_width z)
>    putBig (xcreatewindow_height z)
>    putBig (xcreatewindow_border_width z)
>    putBig (xcreatewindow_value_mask z)
>    put (xcreatewindow_value_list z)

>mapWindowPut z = putBig (xr_wid z)

>data XRequest = XCreateWindow {
>     xr_wid :: Window,
>     xr_parent :: Window,
>     xcreatewindow_depth :: CARD8,
>     xcreatewindow_visual :: VisualID,
>     xcreatewindow_x :: Int16,
>     xcreatewindow_y :: Int16,
>     xcreatewindow_width :: CARD16,
>     xcreatewindow_height :: CARD16,
>     xcreatewindow_border_width :: CARD16,
>     xcreatewindow_value_mask :: BITMASK,
>     xcreatewindow_value_list :: [Value] }
>  | XChangeWindowAttributes {
>      xr_wid :: Window,
>      xchangewindowattributes_value_mask :: BITMASK,
>      xchangewindowattributes_value_list :: [Value] }
>  | XGetWindowAttributes {
>      xr_wid :: Window }
>  | XDestroyWindow { xr_wid :: Window }
>  | XDestroySubwindows { xr_wid :: Window }
>  | XChangeSaveSet {
>      xr_wid :: Window,
>      xchangesaveset_mode :: XChangeSaveSetMode }
>  | XReparentWindow {
>      xr_wid :: Window,
>      xr_parent :: Window,
>      xreparent_window_x :: Int16,
>      xreparent_window_y :: Int16 }
>  | XMapWindow { xr_wid :: Window }
>  | XMapSubwindows { xr_wid :: Window }
>  | XUnmapWindow { xr_wid :: Window }
>  | XUnmapSubwindows { xr_wid :: Window }
>  | XConfigureWindow { xr_wid :: Window,
>     xr_value_mask :: BITMASK,
>     xr_value_list :: [Value] }
>  | XCirculateWindow { xr_wid :: Window,
>                       xr_direction :: Direction }
>  | XGetGeometry { xr_drawable :: Drawable }
>  | XQueryTree { xr_wid :: Window }
>  | XInternAtom { xr_name :: STRING8,
>                  xr_only_if_exists :: Bool }
>  | XGetAtomName { xr_atom :: Atom }
>  | XChangeProperty {
>      xr_wid :: Window,
>      xr_property :: Atom,
>      xr_changeproperty_type :: Atom,
>      xr_changeproperty_format :: Int, -- 8,16,32
>      xr_changeproperty_mode :: ChangeProperty_Mode,
>      xr_changeproperty_data :: Either [INT8] (Either [Int16] [Int32]) }
>  | XDeleteProperty {
>      xr_wid :: Window,
>      xr_property :: Atom }
>  | XGetProperty {
>      xr_wid :: Window,
>      xr_property :: Atom,
>      xr_getproperty_type :: Maybe Atom,
>      xr_getproperty_long_offset :: CARD32,
>      xr_getproperty_long_length :: CARD32,
>      xr_delete :: Bool }
>  | XRotateProperties {
>      xr_wid :: Window,
>      xr_delta :: Int16,
>      xr_properties :: [Atom] }
>  | XListProperties {
>      xr_wid :: Window }
>  | XSetSelectionOwner {
>      xr_selection :: Atom,
>      xr_owner :: Maybe Window,
>      xr_time  :: Maybe Timestamp }
>  | XGetSelectionOwner { xr_selection :: Atom }
>  | XConvertSelection {
>      xr_convertselection_selection :: Atom,
>      xr_convertselection_target :: Atom,
>      xr_convertselection_property :: Maybe Atom,
>      xr_convertselection_requestor :: Window,
>      xr_convertselection_time :: Maybe Timestamp }
>  | XSendEvent {
>      xr_sendevent_destination :: SendEvent_Destination,
>      xr_sendevent_propagate :: Bool,
>      xr_sendevent_event_mask :: SetOfEvent,
>      xr_sendevent_event :: Event }
>  | XGrabPointer {
>      xr_wid :: Window,
>      xr_grabpointer_owner_events :: Bool,
>      xr_grabpointer_event_mask :: SetOfPointerEvent,
>      xr_grabpointer_pointer_mode :: GrabPointer_Mode,
>      xr_grabpointer_keyboard_mode :: GrabPointer_Mode,
>      xr_grabpointer_confine_to :: Maybe Window,
>      xr_grabpointer_cursor :: Maybe Cursor,
>      xr_grabpointer_time :: Maybe Timestamp }
>  | XUngrabPointer { xr_ungrabpointer_time :: Maybe Timestamp }
>  | XGrabButton {
>      xr_grabbutton_modifiers :: Maybe (SetOfKeyMask),
>      xr_grabbutton_button :: Maybe Button,
>      xr_grabbutton_grab_window :: Window,
>      xr_grabbutton_owner_events :: Bool,
>      xr_grabbutton_event_mask :: SetOfPointerEvent,
>      xr_grabbutton_pointer_mode :: GrabPointer_Mode,
>      xr_grabbutton_keyboard_mode :: GrabPointer_Mode,
>      xr_grabbutton_confine_to :: Maybe Window,
>      xr_grabbutton_cursor :: Maybe Cursor }
>   | XUngrabButton {
>      xr_ungrabbutton_modifiers :: Maybe (SetOfKeyMask),
>      xr_ungrabbutton_button :: Maybe Button,
>      xr_wid :: Window }
>   | XChangeActivePointerGrab {
>      xr_event_mask :: SetOfPointerEvent,
>      xr_cursor :: Maybe Cursor,
>      xr_time :: Maybe Timestamp }
>   | XGrabKeyboard {
>      xr_wid :: Window,
>      xr_owner_events :: Bool,
>      xr_pointer_mode :: GrabPointer_Mode,
>      xr_keyboard_mode :: GrabPointer_Mode,
>      xr_time :: Maybe Timestamp }
>   | XUngrabKeyboard {
>      xr_time :: Maybe Timestamp }
>   | XGrabKey {
>      xr_grabkey_key :: Maybe KeyCode,
>      xr_grabkey_modifiers :: Maybe (SetOfKeyMask),
>      xr_wid :: Window,
>      xr_owner_events :: Bool,
>      xr_pointer_mode :: GrabPointer_Mode,
>      xr_keyboard_mode :: GrabPointer_Mode }
>   | XUngrabKey {
>      xr_ungrabkey_keycode :: Maybe KeyCode,
>      xr_modifiers :: Maybe (SetOfKeyMask),
>      xr_wid :: Window }
>   | XAllowEvents {
>      xr_allowevents_mode :: AllowEvents_Mode,
>      xr_time :: Maybe Timestamp }
>   | XGrabServer
>   | XUngrabServer
>   | XQueryPointer { xr_wid :: Window }
>   | XGetMotionEvents { xr_wid :: Window,
>                        xr_getmotionevents_start :: Maybe Timestamp,
>                        xr_getmotionevents_stop :: Maybe Timestamp }
>   | XTranslateCoordinates {
>       xr_src_window :: Window,
>       xr_dst_window :: Window,
>       xr_src_x :: Int16,
>       xr_src_y :: Int16 }
>   | XWarpPointer {
>       xr_warppointer_src_window :: Maybe Window,
>       xr_warppointer_dst_window :: Maybe Window,
>       xr_warppointer_src_x :: Int16,
>       xr_warppointer_src_y :: Int16,
>       xr_warppointer_src_width :: CARD16,
>       xr_warppointer_src_height :: CARD16,
>       xr_warppointer_dst_x :: Int16,
>       xr_warppointer_dst_y :: Int16 }
>  | XSetInputFocus {
>      xr_focus :: InputFocus,
>      xr_revert_to :: InputFocus_RevertTo,
>      xr_time :: Maybe Timestamp }
> | XGetInputFocus
> | XQueryKeymap
> | XOpenFont { xr_fid :: Font, xr_name :: STRING8 }
> | XCloseFont { xr_fid :: Font }
> | XQueryFont -- TODO
> | XQueryTextExtents
> | XListFonts
> | XListFontsWithInfo
> | XSetFontPath
> | XGetFontPath
> | XCreatePixmap
> | XFreePixmap
> | XCreateGC
> | XChangeGC
> | XCopyGC
> | XSetDashes
> | XSetClipRectangles
> | XFreeGC
> | XClearArea
> | XCopyArea
> | XCopyPlane
> | XPolyPoint
> | XPolyLine
> | XPolySegment
> | XPolyRectangle
> | XPolyArc
> | XFillPoly
> | XPolyFillRectangle
> | XPolyFillArc
> | XPutImage
> | XGetImage
> | XPolyText8
> | XPolyText16
> | XImageText8
> | XImageText16
> | XCreateColormap
> | XFreeColormap
> | XCopyColormapAndFree
> | XInstallColormap
> | XUninstallColormap
> | XListInstalledColormaps
> | XAllocColor
> | XAllocNamedColor
> | XAllocColorCells
> | XAllocColorPlanes
> | XFreeColors
> | XStoreColors
> | XStoreNamedColor
> | XQueryColors
> | XLookupColor
> | XCreateCursor
> | XCreateGlyphCursor
> | XFreeCursor
> | XRecolorCursor
> | XQueryBestSize
> | XQueryExtension
> | XListExtensions
> | XSetModifierMapping
> | XGetModifierMapping
> | XChangeKeyboardMapping
> | XGetKeyboardMapping
> | XChangeKeyboardControl
> | XGetKeyboardControl
> | XBell
> | XSetPointerMapping
> | XGetPointerMapping
> | XChangePointerControl
> | XGetPointerControl
> | XSetScreenSaver
> | XGetScreenSaver
> | XForceScreenSaver
> | XChangeHosts
> | XListHosts
> | XSetAccessControl
> | XSetCloseDownMode
> | XKillClient
> | XNoOperation
> -- TODO: QueryFont
>  deriving (Show)
>     
>data AllowEvents_Mode = AEM_AsyncPointer | AEM_SyncPointer | AEM_ReplayPointer
>                      | AEM_AsyncKeyboard | AEM_SyncKeyboard
>                      | AEM_ReplayKeyboard | AEM_AsyncBoth
>                      | AEM_SyncBoth
>   deriving (Show)
>data GrabPointer_Mode = GrabPointer_Mode_Synchronous | GrabPointer_Mode_Asynchronous
>  deriving (Show)
>data SendEvent_Destination = SendEvent_Destination_Window Window
>                           | SendEvent_Destination_PointerWindow
>                           | SendEvent_Destination_InputFocus
>  deriving (Show)
>data ChangeProperty_Mode = ChangeProperty_Mode_Replace
>                         | ChangeProperty_Mode_Prepend
>                         | ChangeProperty_Mode_Append
>   deriving (Show)
>data Direction = DirectionRaiseLowest | DirectionLowerHighest
>   deriving (Show)
>data XChangeSaveSetMode = XChangeSaveSetMode_Insert | XChangeSaveSetMode_Delete
>   deriving (Show)
>data XWindowAttributesClass = XWAttributesClass_InputOutput
>  | XWAttributesClass_InputOnly
>   deriving (Show)
>data XWindowAttributesMapState = XWAttributesMapState_Unmapped
> | XWAttributesMapState_Unviewable | XWAttributesMapState_Viewable
>   deriving (Show)
>data XRequestResponse = XRRError { xrr_error :: XError }
>     | XRRCreateWindow 
>     | XRRChangeWindowAttributes
>     | XRRGetWindowAttributes {
>         xrr_getwindowattributes_visual :: VisualID,
>         xrr_getwindowattributes_class  :: XWindowAttributesClass,
>         xrr_getwindowattributes_bitgravity :: BitGravity,
>         xrr_getwindowattributes_wingravity :: WinGravity,
>         xrr_getwindowattributes_backing_store :: XWABackingStore,
>         xrr_getwindowattributes_backing_planes :: CARD32,
>         xrr_getwindowattributes_backing_pixel :: CARD32,
>         xrr_getwindowattributes_save_under :: Bool,
>         xrr_getwindowattributes_colormap :: Maybe Colormap,
>         xrr_getwindowattributes_map_is_installed :: Bool,
>         xrr_getwindowattributes_map_state  :: XWindowAttributesMapState,
>         xrr_getwindowattributes_all_event_masks :: [Event],
>         xrr_getwindowattributes_your_event_mask :: [Event],
>         xrr_getwindowattributes_do_not_propagate_mask :: SetOfDeviceEvent,
>         xrr_override_redirect :: Bool }
>     | XRRDestroyWindow
>     | XRRDestroySubwindows
>     | XRRChangeSaveSet
>     | XRRReparentWindow
>     | XRRMapWindow
>     | XRRMapSubwindows
>     | XRRUnmapWindow
>     | XRRConfigureWindow
>     | XRRCirculateWindow
>     | XRRGetGeometry {
>          xrr_getgeometry_root :: Window,
>          xrr_getgeometry_depth :: CARD8,
>          xrr_getgeometry_x :: INT16,
>          xrr_getgeometry_y :: INT16,
>          xrr_getgeometry_width :: CARD16,
>          xrr_getgeometry_height :: CARD16,
>          xrr_getgeometry_border_width :: CARD16 }
>     | XRRQueryTree {
>          xrr_querytree_root :: Window,
>          xrr_querytree_parent :: Maybe Window,
>          xrr_querytree_children :: [Window] }
>     | XRRInternAtom { xrr_atom :: Maybe Atom }
>     | XRRGetAtomName { xrr_getatom_name :: STRING8 }
>     | XRRChangeProperty
>     | XRRDeleteProperty
>     | XRRGetProperty {
>          xrr_getproperty_type :: Maybe Atom,
>          xrr_getproperty_format :: Int, -- 0,8,16,32
>          xrr_getproperty_bytes_after :: CARD32,
>          xrr_getproperty_value :: Either [Int8] (Either [Int16] [Int32]) }
>     | XRRRotateProperties
>     | XRRListProperties { xrr_list_properties_atoms :: [Atom] }
>     | XRRSetSelectionOwner
>     | XRRGetSelectionOwner { xrr_owner :: Maybe Window }
>     | XRRConvertSelection
>     | XRRSendEvent
>     | XRRGrabPointer { xrr_grabpointer_status :: GrabPointer_Status }
>     | XRRUngrabPointer
>     | XRRGrabButton
>     | XRRUngrabButton
>     | XRRChangeActivePointerGrab
>     | XRRGrabKeyboard { xrr_grabkeyboard_status :: GrabPointer_Status }
>     | XRRAllowEvents
>     | XRRGrabServer
>     | XRRUngrabServer
>     | XRRQueryPointer {
>         xrr_root :: Window,
>         xrr_child :: Maybe Window,
>         xrr_querypointer_same_screen :: Bool,
>         xrr_querypointer_root_x :: Int16,
>         xrr_querypointer_root_y :: Int16,
>         xrr_querypointer_win_x :: Int16,
>         xrr_querypointer_win_y :: Int16,
>         xrr_querypointer_mask :: SetOfKeyButMask }
>     | XRRGetMotionEvents { xrr_getmotionevents_events :: [TimeCoord] }
>     | XRRTranslateCoordinates {
>         xrr_translatecoordinates_same_screen :: Bool,
>         xrr_translatecoordinates_child :: Maybe Window,
>         xrr_translatecoordinates_dst_x :: Int16,
>         xrr_translatecoordinates_dst_y :: Int16 }
>     | XRRWarpPointer
>     | XRRGetInputFocus { xrr_focus :: InputFocus,
>                          xrr_revert_to :: InputFocus_RevertTo }
>     | XRRQueryKeymap { xrr_keys :: [CARD8] }
>     | XRROpenFont
>     | XRRCloseFont
>  deriving (Show)
>data InputFocus = InputFocus_Window Window | InputFocus_PointerRoot
>                                           | InputFocus_None
>  deriving (Show, Eq)
>data InputFocus_RevertTo = RevertTo_Parent | RevertTo_PointerRoot
>                                           | RevertTo_None
>  deriving (Show, Eq)
>data TimeCoord = TimeCoord { timecoord_x :: Int16,
>                             timecoord_y :: Int16,
>                             timecoord_time :: Timestamp }
>  deriving (Show)
>data GrabPointer_Status = GPS_Success | GPSAlreadyGrabbed | GPSFrozen
>   | GPSInvalidTime | GPSNotViewable
>  deriving (Show, Eq)
