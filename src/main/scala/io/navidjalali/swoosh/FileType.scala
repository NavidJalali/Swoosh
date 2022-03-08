package io.navidjalali.swoosh

import io.navidjalali.swoosh.parse.Parser

trait FileType {
  val parser: Parser[Unit]

  val extensions: Set[String]

  val priority: Int

  val mimeTypes: Set[String]
}

object FileType {
  /**
   * Script or data to be passed to the program following the shebang (#!)
   */
  case object Script extends FileType {
    override val parser =
      Parser.sequence(Array(35, 33).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 2

    override val mimeTypes = Set.empty
  }

  /**
   * Libpcap File Format
   */
  case object Libpcap extends FileType {
    override val parser =
      Parser.sequence(Array(-44, -61, -78, -95).map(_.toByte)).unit()

    override val extensions = Set("pcap")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.tcpdump.pcap")
  }

  /**
   * Libpcap File Format (nanosecond-resolution)
   */
  case object LibpcapNanoSecondResolution extends FileType {
    override val parser =
      Parser.sequence(Array(77, 60, -78, -95).map(_.toByte)).unit()

    override val extensions = Set("pcap")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.tcpdump.pcap")
  }

  /**
   * PCAP Next Generation Dump File Format
   */
  case object PCAPNextGeneration extends FileType {
    override val parser =
      Parser.sequence(Array(10, 13, 13, 10).map(_.toByte)).unit()

    override val extensions = Set("pcapng")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * RedHat Package Manager (RPM) package
   */
  case object RPM extends FileType {
    override val parser =
      Parser.sequence(Array(-19, -85, -18, -37).map(_.toByte)).unit()

    override val extensions = Set("rpm")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * SQLite Database
   */
  case object SQLiteDatabase extends FileType {
    override val parser =
      Parser.sequence(Array(83, 81, 76, 105, 116, 101, 32, 102, 111, 114, 109, 97, 116, 32, 51, 0).map(_.toByte)).unit()

    override val extensions = Set("sqlitedb", "sqlite", "db")

    override val priority = 16

    override val mimeTypes = Set.empty
  }

  /**
   * Amazon Kindle Update Package
   */
  case object AmazonKindleUpdatePackage extends FileType {
    override val parser =
      Parser.sequence(Array(83, 80, 48, 49).map(_.toByte)).unit()

    override val extensions = Set("bin")

    override val priority = 4

    override val mimeTypes = Set("application/octet-stream")
  }

  /**
   * PalmPilot Database/Document File
   */
  case object PalmPilotDatabase extends FileType {
    override val parser =
      for {
        _ <- Parser.anyByte.repeat(11)
        _ <- Parser.sequence(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("PDB")

    override val priority = 35

    override val mimeTypes = Set.empty
  }

  /**
   * Palm Desktop To Do Archive
   */
  case object PalmDesktopToDoArchive extends FileType {
    override val parser =
      Parser.sequence(Array(0, 1, 66, 68).map(_.toByte))
        .orElse(Parser.sequence(Array(-66, -70, -2, -54).map(_.toByte)))
        .unit()

    override val extensions = Set("DBA")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Palm Desktop Calendar Archive
   */
  case object PalmDesktopCalendarArchive extends FileType {
    override val parser =
      Parser.sequence(Array(0, 1, 68, 84).map(_.toByte)).unit()

    override val extensions = Set("TDA")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Telegram Desktop File
   */
  case object TelegramDesktopFile extends FileType {
    override val parser =
      Parser.sequence(Array(84, 68, 70, 36).map(_.toByte)).unit()

    override val extensions = Set("TDF$")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Telegram Desktop Encrypted File
   */
  case object TelegramDesktopEncryptedFile extends FileType {
    override val parser =
      Parser.sequence(Array(84, 68, 69, 70).map(_.toByte)).unit()

    override val extensions = Set("TDEF")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Palm Desktop Data File (Access format)
   */
  case object PalmDesktopDataFile extends FileType {
    override val parser =
      Parser.sequence(Array(0, 1, 0, 0).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Computer icon encoded in ICO file format
   */
  case object Icon extends FileType {
    override val parser =
      Parser.sequence(Array(0, 0, 1, 0).map(_.toByte)).unit()

    override val extensions = Set("ico")

    override val priority = 4

    override val mimeTypes = Set("image/x-icon")
  }
  // COULD NOT GENERATE[Expected 3B20 but found 6537] AppleIcon; 69636e73; 0; [icns]; Apple Icon Image format

  /**
   * 3rd Generation Partnership Project 3GPP and 3GPP2 multimedia files
   */
  case object ThirdGenerationPartnershipProject extends FileType {
    override val parser =
      for {
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(102, 116, 121, 112, 51, 103).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("3gp", "3g2")

    override val priority = 10

    override val mimeTypes = Set("video/3gpp", "video/3gpp2")
  }

  /**
   * compressed file (often tar zip) using Lempel-Ziv-Welch algorithm
   */
  case object LempelZivWelchCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(31, -99).map(_.toByte)).unit()

    override val extensions = Set("z", "tar.z")

    override val priority = 2

    override val mimeTypes = Set.empty
  }

  /**
   * Compressed file (often tar zip) using LZH algorithm
   */
  case object LZHCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(31, -96).map(_.toByte)).unit()

    override val extensions = Set("z", "tar.z")

    override val priority = 2

    override val mimeTypes = Set.empty
  }

  /**
   * AmiBack Amiga Backup data file
   */
  case object AmiBackAmigaBackup extends FileType {
    override val parser =
      Parser.sequence(Array(66, 65, 67, 75, 77, 73, 75, 69, 68, 73, 83, 75).map(_.toByte)).unit()

    override val extensions = Set("bac")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * AmiBack Amiga Backup index file
   */
  case object AmiBackAmigaBackupIndex extends FileType {
    override val parser =
      Parser.sequence(Array(73, 78, 68, 88).map(_.toByte)).unit()

    override val extensions = Set("idx")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Binary Property List file
   */
  case object BinaryPropertyList extends FileType {
    override val parser =
      Parser.sequence(Array(98, 112, 108, 105, 115, 116).map(_.toByte)).unit()

    override val extensions = Set("plist")

    override val priority = 6

    override val mimeTypes = Set.empty
  }

  /**
   * Compressed file using Bzip2 algorithm
   */
  case object Bzip2Compressed extends FileType {
    override val parser =
      Parser.sequence(Array(66, 90, 104).map(_.toByte)).unit()

    override val extensions = Set("bz2")

    override val priority = 3

    override val mimeTypes = Set("application/x-bzip2")
  }

  /**
   * Image file encoded in the Graphics Interchange Format (GIF)
   */
  case object GIF extends FileType {
    override val parser =
      Parser.sequence(Array(71, 73, 70, 56, 55, 97).map(_.toByte)).unit()

    override val extensions = Set("gif")

    override val priority = 6

    override val mimeTypes = Set("image/gif")
  }

  /**
   * Tagged Image File Format (TIFF)
   */
  case object TIFF extends FileType {
    override val parser =
      Parser.sequence(Array(73, 73, 42, 0).map(_.toByte))
        .orElse(Parser.sequence(Array(77, 77, 0, 42).map(_.toByte)))
        .unit()

    override val extensions = Set("tif", "tiff")

    override val priority = 4

    override val mimeTypes = Set("image/tiff")
  }

  /**
   * Canon RAW Format Version 2, Canon's RAW format is based on TIFF
   */
  case object CanonRAWFormatVersion2 extends FileType {
    override val parser =
      Parser.sequence(Array(73, 73, 42, 0, 16, 0, 0, 0, 67, 82).map(_.toByte)).unit()

    override val extensions = Set("cr2")

    override val priority = 10

    override val mimeTypes = Set.empty
  }

  /**
   * Kodak Cineon image
   */
  case object KodakCineonImage extends FileType {
    override val parser =
      Parser.sequence(Array(-128, 42, 95, -41).map(_.toByte)).unit()

    override val extensions = Set("cin")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Compressed file using Rob Northen Compression (version 1 and 2) algorithm
   */
  case object RobNorthenCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(82, 78, 67, 1).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * nuru ASCII/ANSI image and palette files
   */
  case object NuruAscii extends FileType {
    override val parser =
      Parser.sequence(Array(78, 85, 82, 85, 73, 77, 71).map(_.toByte)).unit()

    override val extensions = Set("nui", "nup")

    override val priority = 7

    override val mimeTypes = Set.empty
  }

  /**
   * SMPTE DPX image
   */
  case object DPX extends FileType {
    override val parser =
      Parser.sequence(Array(83, 68, 80, 88).map(_.toByte)).unit()

    override val extensions = Set("dpx")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * OpenEXR image
   */
  case object OpenEXR extends FileType {
    override val parser =
      Parser.sequence(Array(118, 47, 49, 1).map(_.toByte)).unit()

    override val extensions = Set("exr")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Better Portable Graphics format
   */
  case object BPG extends FileType {
    override val parser =
      Parser.sequence(Array(66, 80, 71, -5).map(_.toByte)).unit()

    override val extensions = Set("bpg")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * JPEG raw or in the JFIF or Exif file format
   */
  case object JPEG extends FileType {
    override val parser =
      (for {
        _ <- Parser.sequence(Array(-1, -40, -1, -31).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(2)
        _ <- Parser.sequence(Array(69, 120, 105, 102, 0, 0).map(_.toByte)).unit()
      } yield ())
        .orElse(Parser.sequence(Array(-1, -40, -1, -32, 0, 16, 74, 70, 73, 70, 0, 1).map(_.toByte)))
        .orElse(Parser.sequence(Array(-1, -40, -1, -37).map(_.toByte)))
        .orElse(Parser.sequence(Array(-1, -40, -1, -18).map(_.toByte)))
        .unit()

    override val extensions = Set("jpg", "jpeg")

    override val priority = 12

    override val mimeTypes = Set("image/jpeg")
  }

  /**
   * JPEG 2000 format
   */
  case object JPEG2000 extends FileType {
    override val parser =
      Parser.sequence(Array(0, 0, 0, 12, 106, 80, 32, 32, 13, 10, -121, 10).map(_.toByte))
        .orElse(Parser.sequence(Array(-1, 79, -1, 81).map(_.toByte)))
        .unit()

    override val extensions = Set("jp2", "j2k", "jpf", "jpm", "jpg2", "j2c", "jpc", "jpx", "mj2")

    override val priority = 12

    override val mimeTypes = Set("video/jpm", "video/mj2")
  }

  /**
   * IFF Interleaved Bitmap Image
   */
  case object IFFInterleavedBitmapImage extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(73, 76, 66, 77).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("4B57414A", "lbm", "ibm", "iff")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * IFF 8-Bit Sampled Voice
   */
  case object IFF8BitSampledVoice extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(56, 83, 86, 88).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("8svx", "8sv", "svx", "snd", "iff")

    override val priority = 12

    override val mimeTypes = Set("audio/basic")
  }

  /**
   * Amiga Contiguous Bitmap
   */
  case object AmigaContiguousBitmap extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(65, 67, 66, 77).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("acbm", "iff")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * IFF Animated Bitmap
   */
  case object IFFAnimatedBitmap extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(65, 78, 66, 77).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("anbm", "iff")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * IFF CEL Animation
   */
  case object IFFCELAnimation extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(65, 78, 73, 77).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("anim", "iff")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * IFF Facsimile Image
   */
  case object IFFFacsimileImage extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(70, 65, 88, 88).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("faxx", "fax", "iff")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * IFF Formatted Text
   */
  case object IFFFormattedText extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(70, 84, 88, 84).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("ftxt", "iff")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * IFF Simple Musical Score
   */
  case object IFFSimpleMusicalScore extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(83, 77, 85, 83).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("smus", "smu", "mus", "iff")

    override val priority = 12

    override val mimeTypes = Set("application/vnd.musician")
  }

  /**
   * IFF Musical Score
   */
  case object IFFMusicalScore extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(67, 77, 85, 83).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("cmus", "mus", "iff")

    override val priority = 12

    override val mimeTypes = Set("application/vnd.musician")
  }

  /**
   * IFF YUV Image
   */
  case object IFFYUVImage extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(89, 85, 86, 78).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("yuvn", "yuv", "iff")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * Amiga Fantavision Movie
   */
  case object AmigaFantavisionMovie extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(70, 65, 78, 84).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("iff")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * Audio Interchange File Format
   */
  case object AudioInterchange extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(65, 73, 70, 70).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("aiff", "aif", "aifc", "snd", "iff")

    override val priority = 12

    override val mimeTypes = Set("audio/x-aiff", "audio/basic")
  }

  /**
   * lzip compressed file
   */
  case object LzipCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(76, 90, 73, 80).map(_.toByte)).unit()

    override val extensions = Set("lz")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * DOS MZ executable and its descendants (including NE and PE)
   */
  case object DOSExecutable extends FileType {
    override val parser =
      Parser.sequence(Array(77, 90).map(_.toByte))
        .orElse(Parser.sequence(Array(90, 77).map(_.toByte)))
        .unit()

    override val extensions = Set("exe", "scr", "sys", "dll", "fon", "cpl", "iec", "ime", "rs", "tsp", "mz")

    override val priority = 2

    override val mimeTypes = Set("application/x-msdownload", "application/rls-services+xml")
  }

  /**
   * zip file format and formats based on it, such as EPUB, JAR, ODF, OOXML
   */
  case object ZipCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(80, 75, 3, 4).map(_.toByte)).unit()

    override val extensions = Set("zip", "aar", "apk", "docx", "epub", "ipa", "jar", "kmz", "maff", "msix", "odp", "ods", "odt", "pk3", "pk4", "pptx", "usdz", "vsdx", "xlsx", "xpi")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.android.package-archive", "application/zip", "application/vnd.oasis.opendocument.presentation", "application/vnd.google-earth.kmz", "application/vnd.oasis.opendocument.spreadsheet", "application/epub+zip", "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "application/java-archive", "application/x-xpinstall", "application/vnd.oasis.opendocument.text", "application/vnd.openxmlformats-officedocument.presentationml.presentation", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  }

  /**
   * Roshal ARchive compressed archive v5.00 onwards
   */
  case object RarCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(82, 97, 114, 33, 26, 7, 1, 0).map(_.toByte))
        .orElse(Parser.sequence(Array(82, 97, 114, 33, 26, 7, 0).map(_.toByte)))
        .unit()

    override val extensions = Set("rar")

    override val priority = 8

    override val mimeTypes = Set("application/x-rar-compressed")
  }

  /**
   * Executable and Linkable Format
   */
  case object ExecutableAndLinkable extends FileType {
    override val parser =
      Parser.sequence(Array(127, 69, 76, 70).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Image encoded in the Portable Network Graphics format
   */
  case object PNG extends FileType {
    override val parser =
      Parser.sequence(Array(-119, 80, 78, 71, 13, 10, 26, 10).map(_.toByte)).unit()

    override val extensions = Set("png")

    override val priority = 8

    override val mimeTypes = Set("image/png")
  }

  /**
   * Java class file, Mach-O Fat Binary
   */
  case object JavaClass extends FileType {
    override val parser =
      Parser.sequence(Array(-54, -2, -70, -66).map(_.toByte)).unit()

    override val extensions = Set("class")

    override val priority = 4

    override val mimeTypes = Set("application/java-vm")
  }

  /**
   * UTF-8 byte order mark, commonly seen in text files.
   */
  case object UTF8 extends FileType {
    override val parser =
      Parser.sequence(Array(-17, -69, -65).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * UTF-16LE byte order mark, commonly seen in text files.
   */
  case object UTF16LE extends FileType {
    override val parser =
      Parser.sequence(Array(-1, -2).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 2

    override val mimeTypes = Set.empty
  }

  /**
   * UTF-16BE byte order mark, commonly seen in text files.
   */
  case object UTF16BE extends FileType {
    override val parser =
      Parser.sequence(Array(-2, -1).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 2

    override val mimeTypes = Set.empty
  }

  /**
   * UTF-32LE byte order mark for text
   */
  case object UTF32LE extends FileType {
    override val parser =
      Parser.sequence(Array(-1, -2, 0, 0).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * UTF-32BE byte order mark for text
   */
  case object UTF32BE extends FileType {
    override val parser =
      Parser.sequence(Array(0, 0, -2, -1).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * UTF-7 byte order mark for text
   */
  case object UTF7 extends FileType {
    override val parser =
      Parser.sequence(Array(43, 47, 118, 56).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * SCSU byte order mark for text
   */
  case object SCSU extends FileType {
    override val parser =
      Parser.sequence(Array(14, -2, -1).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * UTF-EBCDIC byte order mark for text
   */
  case object UTFEBCDIC extends FileType {
    override val parser =
      Parser.sequence(Array(-35, 115, 102, 115).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }
  // COULD NOT GENERATE[Expected 3B20 but found 7831] MachOBinary32Bit; FEEDFACE; 00x1000; []; Mach-O binary (32-bit)
  // COULD NOT GENERATE[Expected 3B20 but found 7831] MachOBinary64Bit; FEEDFACF; 00x1000; []; Mach-O binary (64-bit)

  /**
   * JKS JavakeyStore
   */
  case object JavaKeyStore extends FileType {
    override val parser =
      Parser.sequence(Array(-2, -19, -2, -19).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * PostScript document
   */
  case object PostScript extends FileType {
    override val parser =
      Parser.sequence(Array(37, 33, 80, 83).map(_.toByte)).unit()

    override val extensions = Set("ps")

    override val priority = 4

    override val mimeTypes = Set("application/postscript")
  }

  /**
   * MS Windows HtmlHelp Data
   */
  case object WindowsHtmlHelp extends FileType {
    override val parser =
      Parser.sequence(Array(73, 84, 83, 70, 3, 0, 0, 0, 96, 0, 0, 0).map(_.toByte)).unit()

    override val extensions = Set("chm")

    override val priority = 12

    override val mimeTypes = Set("application/vnd.ms-htmlhelp")
  }

  /**
   * PDF document
   */
  case object PDF extends FileType {
    override val parser =
      Parser.sequence(Array(37, 80, 68, 70, 45).map(_.toByte)).unit()

    override val extensions = Set("pdf")

    override val priority = 5

    override val mimeTypes = Set("application/pdf")
  }

  /**
   * Advanced Systems Format
   */
  case object AdvancedSystemsFormat extends FileType {
    override val parser =
      Parser.sequence(Array(48, 38, -78, 117, -114, 102, -49, 17, -90, -39, 0, -86, 0, 98, -50, 108).map(_.toByte)).unit()

    override val extensions = Set("asf", "wma", "wmv")

    override val priority = 16

    override val mimeTypes = Set("video/x-ms-asf", "audio/x-ms-wma", "video/x-ms-wmv")
  }

  /**
   * System Deployment Image, a disk image format used by Microsoft
   */
  case object SystemDeploymentImage extends FileType {
    override val parser =
      Parser.sequence(Array(36, 83, 68, 73, 48, 48, 48, 49).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * Ogg, an open source media container format
   */
  case object OGG extends FileType {
    override val parser =
      Parser.sequence(Array(79, 103, 103, 83).map(_.toByte)).unit()

    override val extensions = Set("ogg", "oga", "ogv")

    override val priority = 4

    override val mimeTypes = Set("audio/ogg", "video/ogg")
  }

  /**
   * Photoshop Document file, Adobe Photoshop's native file format
   */
  case object PSD extends FileType {
    override val parser =
      Parser.sequence(Array(56, 66, 80, 83).map(_.toByte)).unit()

    override val extensions = Set("psd")

    override val priority = 4

    override val mimeTypes = Set("image/vnd.adobe.photoshop")
  }

  /**
   * Waveform Audio File Format
   */
  case object WAV extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(82, 73, 70, 70).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(87, 65, 86, 69).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("wav")

    override val priority = 12

    override val mimeTypes = Set("audio/x-wav")
  }

  /**
   * Audio Video Interleave video format
   */
  case object AVI extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(82, 73, 70, 70).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(65, 86, 73, 32).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("avi")

    override val priority = 12

    override val mimeTypes = Set("video/x-msvideo")
  }

  /**
   * Mpeg1 Layer 3
   */
  case object MP3 extends FileType {
    override val parser = Parser.sequence(Array(73, 68, 51).map(_.toByte))
      .orElse(Parser.sequence(Array(-1, -5).map(_.toByte)))
      .unit()

    override val extensions = Set("mp3")

    override val priority = 3

    override val mimeTypes = Set("audio/mpeg")
  }

  /**
   * BMP file, a bitmap format used mostly in the Windows world
   */
  case object BMP extends FileType {
    override val parser =
      Parser.sequence(Array(66, 77).map(_.toByte)).unit()

    override val extensions = Set("bmp", "dib")

    override val priority = 2

    override val mimeTypes = Set("image/bmp")
  }
  // COULD NOT GENERATE[Expected 3B20 but found 7838] ISO; 4344303031; 0x80010x88010x9001; [iso]; ISO9660 CD/DVD image file

  /**
   * Flexible Image Transport System (FITS)
   */
  case object FITS extends FileType {
    override val parser =
      Parser.sequence(Array(83, 73, 77, 80, 76, 69, 32, 32, 61, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 84).map(_.toByte)).unit()

    override val extensions = Set("fits")

    override val priority = 30

    override val mimeTypes = Set.empty
  }

  /**
   * Free Lossless Audio Codec
   */
  case object FLAC extends FileType {
    override val parser =
      Parser.sequence(Array(102, 76, 97, 67).map(_.toByte)).unit()

    override val extensions = Set("flac")

    override val priority = 4

    override val mimeTypes = Set("audio/x-flac")
  }

  /**
   * MIDI sound file
   */
  case object MIDI extends FileType {
    override val parser =
      Parser.sequence(Array(77, 84, 104, 100).map(_.toByte)).unit()

    override val extensions = Set("mid", "midi")

    override val priority = 4

    override val mimeTypes = Set("audio/midi")
  }

  /**
   * Compound File Binary Format, a container format defined by Microsoft COM. It can contain the equivalent of files and directories. It is used by Windows Installer and for documents in older versions of Microsoft Office. It can be used by other programs as well that rely on the COM and OLE API's.
   */
  case object CompoundFileBinary extends FileType {
    override val parser =
      Parser.sequence(Array(-48, -49, 17, -32, -95, -79, 26, -31).map(_.toByte)).unit()

    override val extensions = Set("doc", "xls", "ppt", "msi", "msg")

    override val priority = 8

    override val mimeTypes = Set("application/msword", "application/vnd.ms-excel", "application/vnd.ms-powerpoint", "application/x-msdownload")
  }

  /**
   * Dalvik Executable
   */
  case object DEX extends FileType {
    override val parser =
      Parser.sequence(Array(100, 101, 120, 10, 48, 51, 53, 0).map(_.toByte)).unit()

    override val extensions = Set("dex")

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * VMDK files
   */
  case object VMDK extends FileType {
    override val parser =
      Parser.sequence(Array(75, 68, 77).map(_.toByte)).unit()

    override val extensions = Set("vmdk")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * Google Chrome extension or packaged app
   */
  case object CRX extends FileType {
    override val parser =
      Parser.sequence(Array(67, 114, 50, 52).map(_.toByte)).unit()

    override val extensions = Set("crx")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * FreeHand 8 document
   */
  case object FH8 extends FileType {
    override val parser =
      Parser.sequence(Array(65, 71, 68, 51).map(_.toByte)).unit()

    override val extensions = Set("fh8")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * AppleWorks 5 document
   */
  case object CWK5 extends FileType {
    override val parser =
      Parser.sequence(Array(5, 7, 0, 0, 66, 79, 66, 79, 5, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1).map(_.toByte)).unit()

    override val extensions = Set("cwk")

    override val priority = 22

    override val mimeTypes = Set.empty
  }

  /**
   * AppleWorks 6 document
   */
  case object CWK6 extends FileType {
    override val parser =
      Parser.sequence(Array(6, 7, -31, 0, 66, 79, 66, 79, 6, 7, -31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1).map(_.toByte)).unit()

    override val extensions = Set("cwk")

    override val priority = 22

    override val mimeTypes = Set.empty
  }

  /**
   * Roxio Toast disc image file
   */
  case object Toast extends FileType {
    override val parser =
      Parser.sequence(Array(-117, 69, 82, 2, 0, 0, 0).map(_.toByte))
        .orElse(Parser.sequence(Array(69, 82, 2, 0, 0, 0).map(_.toByte)))
        .unit()

    override val extensions = Set("toast")

    override val priority = 7

    override val mimeTypes = Set.empty
  }
  // COULD NOT GENERATE[Digit expected but not found.] DMG; 6B6F6C79; end–512; [dmg]; Apple Disk Image file

  /**
   * eXtensible ARchive format
   */
  case object XAR extends FileType {
    override val parser =
      Parser.sequence(Array(120, 97, 114, 33).map(_.toByte)).unit()

    override val extensions = Set("xar")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.xara")
  }

  /**
   * Windows Files And Settings Transfer Repository See also USMT 3.0 (Win XP) and USMT 4.0 (Win 7) User Guides
   */
  case object WindowsFilesAndSettingsTransferRepository extends FileType {
    override val parser =
      Parser.sequence(Array(80, 77, 79, 67, 67, 77, 79, 67).map(_.toByte)).unit()

    override val extensions = Set("dat")

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * Nintendo Entertainment System ROM file
   */
  case object NES extends FileType {
    override val parser =
      Parser.sequence(Array(78, 69, 83, 26).map(_.toByte)).unit()

    override val extensions = Set("nes")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * tar archive
   */
  case object TAR extends FileType {
    override val parser =
      for {
        _ <- Parser.anyByte.repeat(257)
        _ <- Parser.sequence(Array(117, 115, 116, 97, 114, 0, 48, 48).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("tar")

    override val priority = 265

    override val mimeTypes = Set("application/x-tar")
  }

  /**
   * OAR file archive format, where?? is the format version.
   */
  case object OAR extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(79, 65, 82).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(1)
      } yield ()


    override val extensions = Set("oar")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Open source portable voxel file
   */
  case object TOX extends FileType {
    override val parser =
      Parser.sequence(Array(116, 111, 120, 51).map(_.toByte)).unit()

    override val extensions = Set("tox")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Magic Lantern Video file
   */
  case object MLV extends FileType {
    override val parser =
      Parser.sequence(Array(77, 76, 86, 73).map(_.toByte)).unit()

    override val extensions = Set("MLV")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Windows Update Binary Delta Compression file
   */
  case object WindowsUpdateBinaryDeltaCompression extends FileType {
    override val parser =
      Parser.sequence(Array(68, 67, 77, 1, 80, 65, 51, 48).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * 7-Zip File Format
   */
  case object SevenZipCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(55, 122, -68, -81, 39, 28).map(_.toByte)).unit()

    override val extensions = Set("7z")

    override val priority = 6

    override val mimeTypes = Set("application/x-7z-compressed")
  }

  /**
   * GZIP compressed file
   */
  case object GzipCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(31, -117).map(_.toByte)).unit()

    override val extensions = Set("gz", "tar.gz")

    override val priority = 2

    override val mimeTypes = Set.empty
  }

  /**
   * XZ compression utility using LZMA2 compression
   */
  case object XZ extends FileType {
    override val parser =
      Parser.sequence(Array(-3, 55, 122, 88, 90, 0).map(_.toByte)).unit()

    override val extensions = Set("xz", "tar.xz")

    override val priority = 6

    override val mimeTypes = Set("application/x-xz")
  }

  /**
   * LZ4 Frame FormatRemark: LZ4 block format does not offer any magic bytes.
   */
  case object LZ4 extends FileType {
    override val parser =
      Parser.sequence(Array(4, 34, 77, 24).map(_.toByte)).unit()

    override val extensions = Set("lz4")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Microsoft Cabinet file
   */
  case object MicrosoftCabinet extends FileType {
    override val parser =
      Parser.sequence(Array(77, 83, 67, 70).map(_.toByte)).unit()

    override val extensions = Set("cab")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.ms-cab-compressed")
  }

  /**
   * Microsoft compressed file in Quantum format, used prior to Windows XP. File can be decompressed using Extract.exe or Expand.exe distributed with earlier versions of Windows. After compression, the last character of the original filename extension is replaced with an underscore, e.g. ‘Setup.exe’ becomes ‘Setup.ex_’.
   */
  case object QuantumCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(83, 90, 68, 68, -120, -16, 39, 51).map(_.toByte)).unit()

    override val extensions = Set("??_")

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * Free Lossless Image Format
   */
  case object FLIF extends FileType {
    override val parser =
      Parser.sequence(Array(70, 76, 73, 70).map(_.toByte)).unit()

    override val extensions = Set("flif")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Matroska media container, including WebM
   */
  case object MatroskaMedia extends FileType {
    override val parser =
      Parser.sequence(Array(26, 69, -33, -93).map(_.toByte)).unit()

    override val extensions = Set("mkv", "mka", "mks", "mk3d", "webm")

    override val priority = 4

    override val mimeTypes = Set("video/x-matroska", "audio/x-matroska", "video/webm")
  }

  /**
   * SEAN : Session Analysis Training file. Also used in compatible software Rpw: Rowperfect for Windows and RP3W: ROWPERFECT3 for Windows.
   */
  case object STG extends FileType {
    override val parser =
      Parser.sequence(Array(77, 73, 76, 32).map(_.toByte)).unit()

    override val extensions = Set("stg")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * DjVu documentThe following byte is either 55 (U) for single-page or 4D (M) for multi-page documents.
   */
  case object DjVu extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(65, 84, 38, 84, 70, 79, 82, 77).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(68, 74, 86).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("djvu", "djv")

    override val priority = 15

    override val mimeTypes = Set("image/vnd.djvu")
  }

  /**
   * DER encoded X.509 certificate
   */
  case object DER extends FileType {
    override val parser =
      Parser.sequence(Array(48, -126).map(_.toByte)).unit()

    override val extensions = Set("der")

    override val priority = 2

    override val mimeTypes = Set("application/x-x509-ca-cert")
  }

  /**
   * DICOM Medical File Format
   */
  case object DCM extends FileType {
    override val parser =
      for {
        _ <- Parser.anyByte.repeat(128)
        _ <- Parser.sequence(Array(68, 73, 67, 77).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("dcm")

    override val priority = 132

    override val mimeTypes = Set.empty
  }

  /**
   * WOFF File Format 1.0
   */
  case object WOFF extends FileType {
    override val parser =
      Parser.sequence(Array(119, 79, 70, 70).map(_.toByte)).unit()

    override val extensions = Set("woff")

    override val priority = 4

    override val mimeTypes = Set("application/x-font-woff")
  }

  /**
   * WOFF File Format 2.0
   */
  case object WOFF2 extends FileType {
    override val parser =
      Parser.sequence(Array(119, 79, 70, 50).map(_.toByte)).unit()

    override val extensions = Set("woff2")

    override val priority = 4

    override val mimeTypes = Set.empty
  }
  // COULD NOT GENERATE[Expected 3B20 but found 6166] XML; 3C3F786D6C20; 0after BOM; [xml]; eXtensible Markup Language
  // COULD NOT GENERATE[Expected 3B20 but found 6166] XML; 3C003F0078006D006C0020; 0after BOM; [xml]; eXtensible Markup Language
  // COULD NOT GENERATE[Expected 3B20 but found 6166] XML; 003C003F0078006D006C0020; 0after BOM; [xml]; eXtensible Markup Language
  // COULD NOT GENERATE[Expected 3B20 but found 6166] XML; 3C0000003F000000780000006D0000006C00000020000000; 0after BOM; [xml]; eXtensible Markup Language
  // COULD NOT GENERATE[Expected 3B20 but found 6166] XML; 0000003C0000003F000000780000006D0000006C00000020; 0after BOM; [xml]; eXtensible Markup Language
  // COULD NOT GENERATE[Expected 3B20 but found 6166] XML; 4C6FA7949340; 0after BOM; [xml]; eXtensible Markup Language

  /**
   * WebAssembly binary format
   */
  case object WASM extends FileType {
    override val parser =
      Parser.sequence(Array(0, 97, 115, 109).map(_.toByte)).unit()

    override val extensions = Set("wasm")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Lepton compressed JPEG image
   */
  case object LEP extends FileType {
    override val parser =
      Parser.sequence(Array(-49, -124, 1).map(_.toByte)).unit()

    override val extensions = Set("lep")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * Adobe Flash .swf
   */
  case object SWF extends FileType {
    override val parser =
      Parser.sequence(Array(67, 87, 83).map(_.toByte))
        .orElse(Parser.sequence(Array(70, 87, 83).map(_.toByte)))
        .unit()

    override val extensions = Set("swf")

    override val priority = 3

    override val mimeTypes = Set("application/x-shockwave-flash")
  }

  /**
   * linux deb file
   */
  case object DEB extends FileType {
    override val parser =
      Parser.sequence(Array(33, 60, 97, 114, 99, 104, 62, 10).map(_.toByte)).unit()

    override val extensions = Set("deb")

    override val priority = 8

    override val mimeTypes = Set("application/x-debian-package")
  }

  /**
   * Google WebP image file, where???????? is the file size. More information on WebP File Header
   */
  case object WEBP extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(82, 73, 70, 70).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(87, 69, 66, 80).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("webp")

    override val priority = 12

    override val mimeTypes = Set("image/webp")
  }

  /**
   * U-Boot / uImage. Das U-Boot Universal Boot Loader.
   */
  case object UBoot extends FileType {
    override val parser =
      Parser.sequence(Array(39, 5, 25, 86).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Rich Text Format
   */
  case object RTF extends FileType {
    override val parser =
      Parser.sequence(Array(123, 92, 114, 116, 102, 49).map(_.toByte)).unit()

    override val extensions = Set("rtf")

    override val priority = 6

    override val mimeTypes = Set("application/rtf")
  }

  /**
   * Microsoft Tape Format
   */
  case object MicrosoftTape extends FileType {
    override val parser =
      Parser.sequence(Array(84, 65, 80, 69).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }
  // COULD NOT GENERATE[Expected 3B20 but found 7842] MPEG 47; 00xBC0x178...(every 188th byte); [ts,tsv,tsa,mpg,mpeg]; MPEG Transport Stream  (MPEG-2 Part 1)
  // COULD NOT GENERATE[Digit expected but not found.] MPEG 000001BA; 0; [m2p,vob,mpg,mpeg]; MPEG Program Stream  (MPEG-1 Part 1 (essentially identical) and MPEG-2 Part 1)

  /**
   * MPEG-1 video and MPEG-2 video  (MPEG-1 Part 2 and MPEG-2 Part 2)
   */
  case object MPEG extends FileType {
    override val parser =
      Parser.sequence(Array(0, 0, 1, -77).map(_.toByte)).unit()

    override val extensions = Set("mpg", "mpeg")

    override val priority = 4

    override val mimeTypes = Set("video/mpeg")
  }

  /**
   * ISO Base Media file (MPEG-4)
   */
  case object MP4 extends FileType {
    override val parser =
      for {
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(102, 116, 121, 112, 105, 115, 111, 109).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("mp4")

    override val priority = 12

    override val mimeTypes = Set("video/mp4")
  }

  /**
   * No Compression (no preset dictionary)
   */
  case object ZLIB extends FileType {
    override val parser =
      Parser.sequence(Array(120, 1).map(_.toByte)).unit()

    override val extensions = Set("zlib")

    override val priority = 2

    override val mimeTypes = Set.empty
  }
  // COULD NOT GENERATE[Digit expected but not found.] ZLIB; 785E; Best speed (no preset dictionary); [zlib]; No Compression (no preset dictionary)
  // COULD NOT GENERATE[Digit expected but not found.] ZLIB; 789C; Default Compression (no preset dictionary); [zlib]; No Compression (no preset dictionary)
  // COULD NOT GENERATE[Digit expected but not found.] ZLIB; 78DA; Best Compression (no preset dictionary); [zlib]; No Compression (no preset dictionary)
  // COULD NOT GENERATE[Digit expected but not found.] ZLIB; 7820; No Compression (with preset dictionary); [zlib]; No Compression (no preset dictionary)
  // COULD NOT GENERATE[Digit expected but not found.] ZLIB; 787D; Best speed (with preset dictionary); [zlib]; No Compression (no preset dictionary)
  // COULD NOT GENERATE[Digit expected but not found.] ZLIB; 78BB; Default Compression (with preset dictionary); [zlib]; No Compression (no preset dictionary)
  // COULD NOT GENERATE[Digit expected but not found.] ZLIB; 78F9; Best Compression (with preset dictionary); [zlib]; No Compression (no preset dictionary)

  /**
   * LZFSE - Lempel-Ziv style data compression algorithm using Finite State Entropy coding. OSS by Apple.
   */
  case object LZFSE extends FileType {
    override val parser =
      Parser.sequence(Array(98, 118, 120, 50).map(_.toByte)).unit()

    override val extensions = Set("lzfse")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Apache ORC (Optimized Row Columnar) file format
   */
  case object ORC extends FileType {
    override val parser =
      Parser.sequence(Array(79, 82, 67).map(_.toByte)).unit()

    override val extensions = Set("orc")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * Apache Avro binary file format
   */
  case object AVRO extends FileType {
    override val parser =
      Parser.sequence(Array(79, 98, 106, 1).map(_.toByte)).unit()

    override val extensions = Set("avro")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * RCFile columnar file format
   */
  case object RC extends FileType {
    override val parser =
      Parser.sequence(Array(83, 69, 81, 54).map(_.toByte)).unit()

    override val extensions = Set("rc")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * PhotoCap Object Templates
   */
  case object PhotoCapObjectTemplates extends FileType {
    override val parser =
      Parser.sequence(Array(101, -121, 120, 86).map(_.toByte)).unit()

    override val extensions = Set("p25", "obt")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * PhotoCap Vector
   */
  case object PCV extends FileType {
    override val parser =
      Parser.sequence(Array(85, 85, -86, -86).map(_.toByte)).unit()

    override val extensions = Set("pcv")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * PhotoCap Template
   */
  case object PhotoCapTemplate extends FileType {
    override val parser =
      Parser.sequence(Array(120, 86, 52).map(_.toByte)).unit()

    override val extensions = Set("pbt", "pdt", "pea", "peb", "pet", "pgt", "pict", "pjt", "pkt", "pmt")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * Apache Parquet columnar file format
   */
  case object ApacheParquetColumnar extends FileType {
    override val parser =
      Parser.sequence(Array(80, 65, 82, 49).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Emulator Emaxsynth samples
   */
  case object EZ2 extends FileType {
    override val parser =
      Parser.sequence(Array(69, 77, 88, 50).map(_.toByte)).unit()

    override val extensions = Set("ez2")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.ezpix-album")
  }

  /**
   * Emulator III synth samples
   */
  case object EZ3 extends FileType {
    override val parser =
      Parser.sequence(Array(69, 77, 85, 51).map(_.toByte)).unit()

    override val extensions = Set("ez3", "iso")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.ezpix-package", "application/x-iso9660-image")
  }

  /**
   * Lua bytecode
   */
  case object LUAC extends FileType {
    override val parser =
      Parser.sequence(Array(27, 76, 117, 97).map(_.toByte)).unit()

    override val extensions = Set("luac")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * macOS file Alias (Symbolic link)
   */
  case object MacOSSymlink extends FileType {
    override val parser =
      Parser.sequence(Array(98, 111, 111, 107, 0, 0, 0, 0, 109, 97, 114, 107, 0, 0, 0, 0).map(_.toByte)).unit()

    override val extensions = Set("alias")

    override val priority = 16

    override val mimeTypes = Set.empty
  }

  /**
   * Microsoft Zone Identifier for URL Security Zones
   */
  case object MicrosoftZoneIdentifier extends FileType {
    override val parser =
      Parser.sequence(Array(91, 90, 111, 110, 101, 84, 114, 97, 110, 115, 102, 101, 114, 93).map(_.toByte)).unit()

    override val extensions = Set("Identifier")

    override val priority = 14

    override val mimeTypes = Set.empty
  }

  /**
   * Email Message var5[citation needed]
   */
  case object EML extends FileType {
    override val parser =
      Parser.sequence(Array(82, 101, 99, 101, 105, 118, 101, 100, 58).map(_.toByte)).unit()

    override val extensions = Set("eml")

    override val priority = 9

    override val mimeTypes = Set("message/rfc822")
  }

  /**
   * Tableau Datasource
   */
  case object TDE extends FileType {
    override val parser =
      Parser.sequence(Array(32, 2, 1, 98, -96, 30, -85, 7, 2, 0, 0, 0).map(_.toByte)).unit()

    override val extensions = Set("tde")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * KDB file
   */
  case object KDB extends FileType {
    override val parser =
      Parser.sequence(Array(55, 72, 3, 2, 0, 0, 0, 0, 88, 53, 48, 57, 75, 69, 89).map(_.toByte)).unit()

    override val extensions = Set("kdb")

    override val priority = 15

    override val mimeTypes = Set.empty
  }

  /**
   * PGP file
   */
  case object PGP extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(-123).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(2)
        _ <- Parser.sequence(Array(3).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("pgp")

    override val priority = 4

    override val mimeTypes = Set("application/pgp-encrypted")
  }

  /**
   * Zstandard compressed file
   */
  case object ZST extends FileType {
    override val parser =
      Parser.sequence(Array(40, -75, 47, -3).map(_.toByte)).unit()

    override val extensions = Set("zst")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * QuickZip rs compressed archive
   */
  case object QuickZipRSCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(82, 83, 86, 75, 68, 65, 84, 65).map(_.toByte)).unit()

    override val extensions = Set("rs")

    override val priority = 8

    override val mimeTypes = Set("application/rls-services+xml")
  }

  /**
   * Smile file
   */
  case object SML extends FileType {
    override val parser =
      Parser.sequence(Array(58, 41, 10).map(_.toByte)).unit()

    override val extensions = Set("sml")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * Preferred Executable Format
   */
  case object PreferredExecutableFormat extends FileType {
    override val parser =
      Parser.sequence(Array(74, 111, 121, 33).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * SubRip File
   */
  case object SRT extends FileType {
    override val parser =
      Parser.sequence(Array(49, 10, 48, 48).map(_.toByte)).unit()

    override val extensions = Set("srt")

    override val priority = 4

    override val mimeTypes = Set("application/x-subrip")
  }

  /**
   * VPK file, used to store game data for some Source Engine games
   */
  case object VPK extends FileType {
    override val parser =
      Parser.sequence(Array(52, 18, -86, 85).map(_.toByte)).unit()

    override val extensions = Set("vpk")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * ACE (compressed file format)
   */
  case object ACE extends FileType {
    override val parser =
      Parser.sequence(Array(42, 42, 65, 67, 69, 42, 42).map(_.toByte)).unit()

    override val extensions = Set("ace")

    override val priority = 7

    override val mimeTypes = Set("application/x-ace-compressed")
  }

  /**
   * ARJ
   */
  case object ARJ extends FileType {
    override val parser =
      Parser.sequence(Array(96, -22).map(_.toByte)).unit()

    override val extensions = Set("arj")

    override val priority = 2

    override val mimeTypes = Set.empty
  }

  /**
   * InstallShield CAB Archive File
   */
  case object InstallShieldCAB extends FileType {
    override val parser =
      Parser.sequence(Array(73, 83, 99, 40).map(_.toByte)).unit()

    override val extensions = Set("cab")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.ms-cab-compressed")
  }

  /**
   * Windows 3.1x Compressed File
   */
  case object Windows3XCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(75, 87, 65, 74).map(_.toByte)).unit()

    override val extensions = Set("??_")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Windows 9x Compressed File
   */
  case object Windows9XCompressed extends FileType {
    override val parser =
      Parser.sequence(Array(83, 90, 68, 68).map(_.toByte)).unit()

    override val extensions = Set("??_")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Zoo (file format)
   */
  case object ZOO extends FileType {
    override val parser =
      Parser.sequence(Array(90, 79, 79).map(_.toByte)).unit()

    override val extensions = Set("zoo")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * Portable bitmap
   */
  case object PBM extends FileType {
    override val parser =
      Parser.sequence(Array(80, 49, 10).map(_.toByte)).unit()

    override val extensions = Set("pbm")

    override val priority = 3

    override val mimeTypes = Set("image/x-portable-bitmap")
  }

  /**
   * Portable Gray Map
   */
  case object PGM extends FileType {
    override val parser =
      Parser.sequence(Array(80, 50, 10).map(_.toByte)).unit()

    override val extensions = Set("pgm")

    override val priority = 3

    override val mimeTypes = Set("image/x-portable-graymap")
  }

  /**
   * Portable Pixmap
   */
  case object PPM extends FileType {
    override val parser =
      Parser.sequence(Array(80, 51, 10).map(_.toByte)).unit()

    override val extensions = Set("ppm")

    override val priority = 3

    override val mimeTypes = Set("image/x-portable-pixmap")
  }

  /**
   * Windows Metafile
   */
  case object WMF extends FileType {
    override val parser =
      Parser.sequence(Array(-41, -51, -58, -102).map(_.toByte)).unit()

    override val extensions = Set("wmf")

    override val priority = 4

    override val mimeTypes = Set("application/x-msmetafile")
  }

  /**
   * XCF (file format)
   */
  case object XCF extends FileType {
    override val parser =
      Parser.sequence(Array(103, 105, 109, 112, 32, 120, 99, 102).map(_.toByte)).unit()

    override val extensions = Set("xcf")

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * X PixMap
   */
  case object XPM extends FileType {
    override val parser =
      Parser.sequence(Array(47, 42, 32, 88, 80, 77, 32, 42, 47).map(_.toByte)).unit()

    override val extensions = Set("xpm")

    override val priority = 9

    override val mimeTypes = Set("image/x-xpixmap")
  }

  /**
   * Advanced Forensics Format
   */
  case object AFF extends FileType {
    override val parser =
      Parser.sequence(Array(65, 70, 70).map(_.toByte)).unit()

    override val extensions = Set("aff")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * EnCase EWF version 2 format
   */
  case object EX01 extends FileType {
    override val parser =
      Parser.sequence(Array(69, 86, 70, 50).map(_.toByte)).unit()

    override val extensions = Set("Ex01")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * EnCase EWF version 1 format
   */
  case object E01 extends FileType {
    override val parser =
      Parser.sequence(Array(69, 86, 70).map(_.toByte)).unit()

    override val extensions = Set("e01")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * qcow file format
   */
  case object QCOW extends FileType {
    override val parser =
      Parser.sequence(Array(81, 70, 73).map(_.toByte)).unit()

    override val extensions = Set("qcow")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * Animated cursor
   */
  case object ANI extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(82, 73, 70, 70).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(65, 67, 79, 78).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("ani")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * .cda file
   */
  case object CDA extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(82, 73, 70, 70).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(67, 68, 68, 65).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("cda")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * Qualcomm PureVoice file format
   */
  case object QCP extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(82, 73, 70, 70).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(81, 76, 67, 77).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("qcp")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * Adobe Shockwave
   */
  case object DCR extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(82, 73, 70, 88).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(70, 71, 68, 77).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("dcr")

    override val priority = 12

    override val mimeTypes = Set("application/x-director")
  }

  /**
   * Macromedia Director file format
   */
  case object MacromediaDirectorFile extends FileType {
    override val parser =
      for {
        _ <- Parser.sequence(Array(82, 73, 70, 88).map(_.toByte)).unit()
        _ <- Parser.anyByte.repeat(4)
        _ <- Parser.sequence(Array(77, 86, 57, 51).map(_.toByte)).unit()
      } yield ()


    override val extensions = Set("dir", "dxr", "drx")

    override val priority = 12

    override val mimeTypes = Set("application/x-director")
  }

  /**
   * Flash Video file
   */
  case object FLV extends FileType {
    override val parser =
      Parser.sequence(Array(70, 76, 86).map(_.toByte)).unit()

    override val extensions = Set("flv")

    override val priority = 3

    override val mimeTypes = Set("video/x-flv")
  }

  /**
   * VirtualBox Virtual Hard Disk file format
   */
  case object VDI extends FileType {
    override val parser =
      Parser.sequence(
        Array(60, 60, 60, 32, 79, 114, 97, 99, 108, 101, 32, 86, 77, 32, 86,
          105, 114, 116, 117, 97, 108, 66, 111, 120, 32, 68, 105, 115, 107,
          32, 73, 109, 97, 103, 101, 32, 62, 62, 62).map(_.toByte)
      ).unit()

    override val extensions = Set("vdi")

    override val priority = 39

    override val mimeTypes = Set.empty
  }

  /**
   * Windows Virtual PC Virtual Hard Disk file format
   */
  case object VHD extends FileType {
    override val parser =
      Parser.sequence(Array(99, 111, 110, 110, 101, 99, 116, 105, 120).map(_.toByte)).unit()

    override val extensions = Set("vhd")

    override val priority = 9

    override val mimeTypes = Set.empty
  }

  /**
   * Windows Virtual PC Windows 8 Virtual Hard Disk file format
   */
  case object VHDX extends FileType {
    override val parser =
      Parser.sequence(Array(118, 104, 100, 120, 102, 105, 108, 101).map(_.toByte)).unit()

    override val extensions = Set("vhdx")

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * Compressed ISO image
   */
  case object ISZ extends FileType {
    override val parser =
      Parser.sequence(Array(73, 115, 90, 33).map(_.toByte)).unit()

    override val extensions = Set("isz")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Direct Access Archive PowerISO
   */
  case object DAA extends FileType {
    override val parser =
      Parser.sequence(Array(68, 65, 65).map(_.toByte)).unit()

    override val extensions = Set("daa")

    override val priority = 3

    override val mimeTypes = Set.empty
  }

  /**
   * Windows Event Viewer file format
   */
  case object EVT extends FileType {
    override val parser =
      Parser.sequence(Array(76, 102, 76, 101).map(_.toByte)).unit()

    override val extensions = Set("evt")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Windows 3.x Program Manager Program Group file format
   */
  case object GRP extends FileType {
    override val parser =
      Parser.sequence(Array(80, 77, 67, 67).map(_.toByte)).unit()

    override val extensions = Set("grp")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * ICC profile
   */
  case object ICM extends FileType {
    override val parser =
      Parser.sequence(Array(75, 67, 77, 83).map(_.toByte)).unit()

    override val extensions = Set("icm")

    override val priority = 4

    override val mimeTypes = Set("application/vnd.iccprofile")
  }

  /**
   * Windows Registry file
   */
  case object DAT extends FileType {
    override val parser =
      Parser.sequence(Array(114, 101, 103, 102).map(_.toByte)).unit()

    override val extensions = Set("dat")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Microsoft Outlook Personal Storage Table file
   */
  case object PST extends FileType {
    override val parser =
      Parser.sequence(Array(33, 66, 68, 78).map(_.toByte)).unit()

    override val extensions = Set("pst")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * 3D model compressed with Google Draco
   */
  case object DRC extends FileType {
    override val parser =
      Parser.sequence(Array(68, 82, 65, 67, 79).map(_.toByte)).unit()

    override val extensions = Set("drc")

    override val priority = 5

    override val mimeTypes = Set.empty
  }

  /**
   * Gridded data (commonly weather observations or forecasts) in the WMO GRIB or GRIB2 format
   */
  case object GriddedData extends FileType {
    override val parser =
      Parser.sequence(Array(71, 82, 73, 66).map(_.toByte)).unit()

    override val extensions = Set("grib", "grib2")

    override val priority = 4

    override val mimeTypes = Set.empty
  }

  /**
   * Blender File Format
   */
  case object BLEND extends FileType {
    override val parser =
      Parser.sequence(Array(66, 76, 69, 78, 68, 69, 82).map(_.toByte)).unit()

    override val extensions = Set("blend")

    override val priority = 7

    override val mimeTypes = Set.empty
  }

  /**
   * Image encoded in the JPEG XL format
   */
  case object JXL extends FileType {
    override val parser =
      Parser.sequence(Array(0, 0, 0, 12, 74, 88, 76, 32, 13, 10, -121, 10).map(_.toByte))
        .orElse(Parser.sequence(Array(-1, 10).map(_.toByte)))
        .unit()

    override val extensions = Set("jxl")

    override val priority = 12

    override val mimeTypes = Set.empty
  }

  /**
   * TrueType font
   */
  case object TrueTypeFont extends FileType {
    override val parser =
      Parser.sequence(Array(0, 1, 0, 0, 0).map(_.toByte)).unit()

    override val extensions = Set("ttf", "tte", "dfont")

    override val priority = 5

    override val mimeTypes = Set("application/x-font-ttf")
  }

  /**
   * OpenType font
   */
  case object OTF extends FileType {
    override val parser =
      Parser.sequence(Array(79, 84, 84, 79).map(_.toByte)).unit()

    override val extensions = Set("otf")

    override val priority = 4

    override val mimeTypes = Set("application/x-font-otf")
  }

  /**
   * Modulefile for Environment Modules
   */
  case object Modulefile extends FileType {
    override val parser =
      Parser.sequence(Array(35, 37, 77, 111, 100, 117, 108, 101).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * Windows Imaging Format file
   */
  case object WindowsImagingFormat extends FileType {
    override val parser =
      Parser.sequence(Array(77, 83, 87, 73, 77, 0, 0, 0, -48, 0, 0, 0, 0).map(_.toByte)).unit()

    override val extensions = Set("wim", "swm", "esd")

    override val priority = 13

    override val mimeTypes = Set.empty
  }

  /**
   * Slob (sorted list of blobs) is a read-only, compressed data store with dictionary-like interface
   */
  case object SLOB extends FileType {
    override val parser =
      Parser.sequence(Array(33, 45, 49, 83, 76, 79, 66, 31).map(_.toByte)).unit()

    override val extensions = Set("slob")

    override val priority = 8

    override val mimeTypes = Set.empty
  }

  /**
   * Serialized Java Data
   */
  case object SerializedJavaData extends FileType {
    override val parser =
      Parser.sequence(Array(-84, -19).map(_.toByte)).unit()

    override val extensions = Set.empty

    override val priority = 2

    override val mimeTypes = Set.empty
  }

  val all =
    Set(
      VHDX, DEB, JPEG, AmiBackAmigaBackup, UBoot, STG, IFFInterleavedBitmapImage, MatroskaMedia, SWF,
      PalmDesktopCalendarArchive, AFF, ZipCompressed, PNG, MP3, AmazonKindleUpdatePackage, XPM,
      TelegramDesktopEncryptedFile, MLV, PGM, RarCompressed, SRT, JPEG2000, OTF, PCV, OpenEXR, LZFSE, MP4, TIFF,
      AudioInterchange, ORC, EML, IFFFormattedText, DEX, SLOB, JavaKeyStore, CDA, VMDK, FH8, GIF,
      AmiBackAmigaBackupIndex, SystemDeploymentImage, ARJ, SML, DAT, ExecutableAndLinkable, EZ2, WAV,
      WindowsHtmlHelp, TAR, PhotoCapTemplate, FLAC, ZLIB, KDB, ThirdGenerationPartnershipProject, DER, EVT,
      IFFCELAnimation, LZHCompressed, LzipCompressed, RC, MicrosoftTape, RobNorthenCompressed, UTF16LE, VDI,
      WindowsImagingFormat, PalmDesktopToDoArchive, EX01, Toast, LempelZivWelchCompressed, ANI,
      WindowsUpdateBinaryDeltaCompression, Modulefile, PreferredExecutableFormat, PDF, Windows9XCompressed, XAR,
      JavaClass, IFFYUVImage, NuruAscii, SerializedJavaData, BMP, JXL, PCAPNextGeneration, ACE, UTF32LE, DAA, DPX,
      CompoundFileBinary, FLV, GRP, TelegramDesktopFile, PBM, AmigaContiguousBitmap, MicrosoftZoneIdentifier,
      LUAC, ApacheParquetColumnar, AmigaFantavisionMovie, RTF, IFFAnimatedBitmap, EZ3, BPG, CanonRAWFormatVersion2,
      AVRO, PGP, DRC, ZOO, UTF7, MicrosoftCabinet, InstallShieldCAB, LZ4, Script, MPEG, PalmPilotDatabase,
      E01, NES, PalmDesktopDataFile, MacOSSymlink, WMF, XCF, LibpcapNanoSecondResolution, QCOW, WASM, Bzip2Compressed,
      WEBP, GriddedData, Icon, IFF8BitSampledVoice, QCP, UTF8, LEP, SevenZipCompressed, UTF32BE, DjVu, GzipCompressed,
      Libpcap, SQLiteDatabase, ZST, RPM, XZ, VHD, TDE, BinaryPropertyList, SCSU, Windows3XCompressed, UTF16BE,
      TrueTypeFont, KodakCineonImage, OGG, DCR, WOFF2, WOFF, PST, DOSExecutable, VPK, AdvancedSystemsFormat, AVI, MIDI,
      PPM, MacromediaDirectorFile, BLEND, CRX, TOX, FITS, QuickZipRSCompressed, PhotoCapObjectTemplates, OAR,
      IFFSimpleMusicalScore, CWK5, CWK6, PostScript, ICM, IFFMusicalScore, ISZ, IFFFacsimileImage, QuantumCompressed,
      WindowsFilesAndSettingsTransferRepository, DCM, UTFEBCDIC, PSD, FLIF
    )

  val byPriority = all.toVector.sortBy(_.priority).reverse
}
