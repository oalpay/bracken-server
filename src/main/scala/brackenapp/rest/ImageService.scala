package brackenapp.rest

import net.liftweb.http._
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.common.{Box, Full,Logger}
import net.liftweb.util.StringHelpers
import com.mongodb.gridfs._
import net.liftweb.mongodb._
import brackenapp.model.{User}
import net.liftweb.http.StreamingResponse
import net.liftweb.http.OkResponse
import net.liftweb.http.InMemoryResponse
import net.liftweb.common.Full
import xml.{NodeSeq, Text}
import brackenapp.model.{Content,Page}

object ImageService extends RestHelper with Logger {

  serve {

    case "upload" :: Nil Post req => {
      for (file <- req.uploadedFiles) {
        println("Received: " + file.fileName)
      }
      OkResponse()
    }
    case "image" :: Nil Post req => {
      User.currentUser match {
        case Full(user) => {
          def saveImage(fph: FileParamHolder) = {
            val imageName = StringHelpers.randomString(16) 
            MongoDB.use(DefaultMongoIdentifier) {
              db =>
                val inputFile = new GridFS(db).createFile(fph.fileStream)
                inputFile.setContentType(fph.mimeType)
                inputFile.setFilename(imageName)
                inputFile.save()
            }
			val imagePath = "/image/" + imageName
			req.param("pageId") foreach {
				Page.setImageSrc(_,fph.name,imagePath )
			}
			req.param("contentId") foreach {
				Content.setImageSrc(_,fph.name,imagePath )
			}
            ("name" -> imagePath) ~ ("type" -> fph.mimeType) ~ ("size" -> fph.length)
          }

          val ojv: Box[JValue] = req.uploadedFiles.map(fph => saveImage(fph)).headOption
          val ajv = ("name" -> "n/a") ~ ("type" -> "n/a") ~ ("size" -> 0L)
          val ret = ojv openOr ajv

          val jr = JsonResponse(ret).toResponse.asInstanceOf[InMemoryResponse]
          InMemoryResponse(jr.data, ("Content-Length", jr.data.length.toString) ::
            ("Content-Type", "text/plain") :: Nil, Nil, 200)
        }
        case _ =>
          UnauthorizedResponse("User session required")
      }
    }

    case "image" :: imageName :: Nil Get req =>
      MongoDB.use(DefaultMongoIdentifier) {
        db =>
          val image = new GridFS(db).findOne(imageName)
          if (image != null) {
            val imageStream = image.getInputStream
            StreamingResponse(imageStream, () => imageStream.close(), image.getLength, ("Content-Type", image.getContentType)::("Cache-Control","max-age=36000") ::("Pragma", ""):: Nil, Nil, 200)
          } else {
            NotFoundResponse("Image not found")
          }
      }

  }
}